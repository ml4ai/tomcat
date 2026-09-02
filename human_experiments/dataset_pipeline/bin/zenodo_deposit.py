#!/usr/bin/env python
"""Create or update the Zenodo deposition for the ToMCAT dataset.

Reads ``zenodo/metadata.json``, creates a draft deposition (or reuses one by
id), sets its metadata, and uploads the given files to its bucket, streaming
from disk so a 19 GB file needs no memory. It never publishes: publishing
mints the DOI and cannot be undone, so that stays a human click on the Zenodo
draft page.

    ZENODO_TOKEN=... ./bin/zenodo_deposit.py --file tomcat-core.db \
        --file tomcat-core.db.sha256 --file zenodo/README.md

    # Sandbox first (separate account and token at sandbox.zenodo.org):
    ZENODO_TOKEN=... ./bin/zenodo_deposit.py --sandbox --file zenodo/README.md

    # Later runs on the same draft:
    ./bin/zenodo_deposit.py --deposition 1234567 --file tomcat-core.db

The token needs the ``deposit:write`` scope (and ``deposit:actions`` only if
you intend to publish from the API, which this script does not).
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import sys
import time
from pathlib import Path

import requests

HERE = Path(__file__).resolve().parent.parent
DEFAULT_METADATA = HERE / "zenodo" / "metadata.json"


class ProgressFile:
    """Wrap a file object so requests streams it and we get a progress line."""

    def __init__(self, path: Path):
        self.path = path
        self.size = path.stat().st_size
        self.fh = open(path, "rb")  # noqa: SIM115 -- closed in close()
        self.sent = 0
        self.started = time.monotonic()
        self.last = 0.0

    def __len__(self):
        return self.size

    def read(self, n=-1):
        chunk = self.fh.read(n)
        self.sent += len(chunk)
        now = time.monotonic()
        if now - self.last > 5 or self.sent == self.size:
            rate = self.sent / max(now - self.started, 1e-6) / 1e6
            print(
                f"  {self.path.name}: {self.sent / 1e9:.2f}/{self.size / 1e9:.2f} GB "
                f"({rate:.0f} MB/s)",
                file=sys.stderr,
                flush=True,
            )
            self.last = now
        return chunk

    def close(self):
        self.fh.close()


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--metadata", type=Path, default=DEFAULT_METADATA)
    parser.add_argument(
        "--file",
        action="append",
        type=Path,
        default=[],
        help="File to upload (repeatable). Replaces a same-named file already "
        "in the draft.",
    )
    parser.add_argument(
        "--deposition", type=int, help="Existing draft deposition id to update."
    )
    parser.add_argument(
        "--sandbox", action="store_true", help="Use sandbox.zenodo.org."
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Validate inputs and print what would be sent; no network calls.",
    )
    args = parser.parse_args()

    metadata = json.loads(args.metadata.read_text())
    for path in args.file:
        if not path.is_file():
            raise SystemExit(f"not a file: {path}")

    base = "https://sandbox.zenodo.org" if args.sandbox else "https://zenodo.org"
    print(f"Zenodo:   {base}")
    print(f"Title:    {metadata['title']}  (version {metadata.get('version', '-')})")
    print(f"Creators: {len(metadata['creators'])}")
    print(f"Files:    {', '.join(str(p) for p in args.file) or '(none)'}")
    if args.dry_run:
        return

    token = os.environ.get("ZENODO_TOKEN")
    if not token:
        raise SystemExit("ZENODO_TOKEN is not set (secretctl copy ZENODO_TOKEN).")
    session = requests.Session()
    session.headers["Authorization"] = f"Bearer {token}"
    api = f"{base}/api/deposit/depositions"

    if args.deposition:
        r = session.get(f"{api}/{args.deposition}")
        r.raise_for_status()
        dep = r.json()
        if dep.get("submitted"):
            raise SystemExit(
                f"deposition {args.deposition} is published; create a new version "
                "on Zenodo and pass that draft's id instead."
            )
        print(f"Updating draft deposition {dep['id']}")
    else:
        r = session.post(api, json={})
        r.raise_for_status()
        dep = r.json()
        print(f"Created draft deposition {dep['id']}")

    r = session.put(f"{api}/{dep['id']}", json={"metadata": metadata})
    if r.status_code >= 400:
        print(r.text, file=sys.stderr)
    r.raise_for_status()
    dep = r.json()
    print("Metadata set.")

    bucket = dep["links"]["bucket"]
    existing = {f["filename"]: f for f in dep.get("files", [])}
    for path in args.file:
        name = path.name
        local_sum = sha256_file(path) if path.stat().st_size < 1 << 30 else None
        if name in existing:
            # Zenodo reports an md5 checksum; for small files we just replace.
            r = session.delete(existing[name]["links"]["self"])
            r.raise_for_status()
            print(f"Replaced existing {name}")
        print(f"Uploading {name} ({path.stat().st_size / 1e9:.2f} GB)")
        body = ProgressFile(path)
        try:
            r = session.put(f"{bucket}/{name}", data=body)
        finally:
            body.close()
        if r.status_code >= 400:
            print(r.text, file=sys.stderr)
        r.raise_for_status()
        info = r.json()
        print(
            f"  stored: {info.get('key')} {info.get('size')} bytes {info.get('checksum')}"
        )
        if local_sum:
            print(f"  local sha256: {local_sum}")

    print()
    print(f"Draft ready for review (not published): {dep['links']['html']}")
    print("Publish from that page once the files and metadata look right.")


if __name__ == "__main__":
    main()
