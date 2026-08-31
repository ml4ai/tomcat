#!/usr/bin/env python
"""Report whether the raw *task* inputs are present on disk for each group session.

"Task data" here means the files the task-table processors read (see
dataset_pipeline/raw/process_*_task_data.py and process_minecraft_data.py):

  rest_state            baseline_tasks/rest_state/*.csv        (exactly one CSV)
  affective             baseline_tasks/affective/individual_*.csv + team_*.csv
  finger_tapping        baseline_tasks/finger_tapping/*.csv
  ping_pong competitive baseline_tasks/ping_pong/competitive*.csv
  ping_pong cooperative baseline_tasks/ping_pong/cooperative*.csv
  minecraft             minecraft/*.metadata

Groups from 2023-04 onward use the *unified XDF* layout instead (the
processors' `process_directory_v2` path), where every baseline task is read
from {group}/lsl/block_1.xdf. For those groups we check for that file.

This is a read-only, sync-readiness check: it does NOT touch the database. It
exists because the task-table build (and, transitively, the signal-label step)
needs these inputs present for every non-ignored group, and the gauss->snail
rsync is still in progress.

Group selection and the v1/v2 split are imported from the project so this
script stays in lockstep with the real pipeline (e.g. should_ignore_directory
already excludes cancelled/duplicate sessions like exp_2023_04_20_14).

Usage:
    # uses settings.experiment_root_dir (from .env) by default:
    .venv/bin/python bin/check_task_data_synced.py
    # or point at a specific group-sessions root:
    .venv/bin/python bin/check_task_data_synced.py /media/snail-ssd/tomcat/data/raw/LangLab/experiments/study_3_pilot/group

Exit status: 0 once every non-ignored group has all *syncable* task inputs
present — i.e. the only remaining gaps are confirmed-permanent holes that do
not exist at the source (see KNOWN_MISSING). 1 while any input that DOES exist
on gauss is still absent locally (rsync in progress). So this can gate a later
`make update_raw`: exit 0 means "the sync is as complete as it will ever get."
"""

import os
import sys
from glob import glob

from dataset_pipeline.common.config import settings
from dataset_pipeline.common.utils import (
    is_directory_with_unified_xdf_files,
    should_ignore_directory,
)

# Confirmed-permanent data holes: (group_session, task) pairs where the input
# genuinely does NOT exist at the source (verified on gauss, the source of
# truth), so no amount of rsync waiting will fill them. These are reported as
# "OK (known hole)" rather than "MISSING" so a clean run (exit 0) means the
# sync is actually complete. Mirrors the codebase's own special-casing.
#
#   exp_2022_12_05_12 / rest_state: rest_state CSV is 0 bytes; the rest_state
#       processor handles this (falls back to mtime) and label_data.py:247
#       explicitly notes "no rest state data ... due to technical issues
#       (Rick's email 2023-07-11)". (Already passes via nonempty=False, listed
#       here for the record.)
#   exp_2023_01_30_13 / minecraft: no minecraft/ dir and zero *.metadata files
#       anywhere in the group ON GAUSS (testbed_logs/ is empty at the source);
#       this session has no Minecraft mission recordings. Verified 2026-05-30.
KNOWN_MISSING = {
    ("exp_2023_01_30_13", "minecraft"): "no minecraft data on gauss (never recorded)",
}


def _has(pattern, *, minimum=1, nonempty=True):
    """True if at least `minimum` files match `pattern` (optionally non-empty)."""
    matches = glob(pattern)
    if nonempty:
        matches = [m for m in matches if os.path.isfile(m) and os.path.getsize(m) > 0]
    return len(matches) >= minimum


def check_group(group_dir):
    """Return {task_label: (ok, detail)} for one group-session directory."""
    results = {}

    if is_directory_with_unified_xdf_files(os.path.basename(group_dir)):
        # v2 / unified-XDF era: all baseline tasks come from lsl/block_1.xdf.
        xdf = os.path.join(group_dir, "lsl", "block_1.xdf")
        ok = os.path.isfile(xdf) and os.path.getsize(xdf) > 0
        detail = "lsl/block_1.xdf" + ("" if ok else " MISSING")
        for label in (
            "rest_state",
            "affective",
            "finger_tapping",
            "ping_pong_competitive",
            "ping_pong_cooperative",
        ):
            results[label] = (ok, detail)
        # minecraft in the unified era is also read from the unified stream;
        # treat the same xdf as the readiness signal.
        results["minecraft"] = (ok, detail)
        return results

    # v1 / pre-unified era: per-task CSVs under baseline_tasks/ + .metadata files.
    bt = os.path.join(group_dir, "baseline_tasks")
    # rest_state: the processor (process_rest_state_task_data.py) globs *.csv,
    # expects exactly one, and explicitly handles a 0-byte file (falls back to
    # the file's mtime). So "present" here means the CSV exists at ALL, any size.
    results["rest_state"] = (
        _has(os.path.join(bt, "rest_state", "*.csv"), nonempty=False),
        "baseline_tasks/rest_state/*.csv",
    )
    ind = _has(os.path.join(bt, "affective", "individual_*.csv"))
    team = _has(os.path.join(bt, "affective", "team_*.csv"))
    results["affective"] = (
        ind and team,
        f"affective individual={'ok' if ind else 'MISSING'} team={'ok' if team else 'MISSING'}",
    )
    results["finger_tapping"] = (
        _has(os.path.join(bt, "finger_tapping", "*.csv")),
        "baseline_tasks/finger_tapping/*.csv",
    )
    results["ping_pong_competitive"] = (
        _has(os.path.join(bt, "ping_pong", "competitive*.csv")),
        "baseline_tasks/ping_pong/competitive*.csv",
    )
    results["ping_pong_cooperative"] = (
        _has(os.path.join(bt, "ping_pong", "cooperative*.csv")),
        "baseline_tasks/ping_pong/cooperative*.csv",
    )
    results["minecraft"] = (
        _has(os.path.join(group_dir, "minecraft", "*.metadata")),
        "minecraft/*.metadata",
    )
    return results


def main():
    root = sys.argv[1] if len(sys.argv) > 1 else settings.experiment_root_dir
    if not os.path.isdir(root):
        print(f"ERROR: group-sessions root does not exist: {root}", file=sys.stderr)
        return 2

    groups = sorted(
        d
        for d in os.listdir(root)
        if os.path.isdir(os.path.join(root, d))
        and d.startswith("exp_")
        and not should_ignore_directory(d)
    )
    if not groups:
        print(f"No (non-ignored) group sessions found under {root}", file=sys.stderr)
        return 2

    print(f"Task-data sync check over {len(groups)} group session(s) under:\n  {root}\n")
    header = f"{'group':<22} {'layout':<6} {'status':<8} missing"
    print(header)
    print("-" * len(header))

    ready = 0
    holes_only = 0  # groups whose only gaps are confirmed-permanent (known holes)
    for g in groups:
        gdir = os.path.join(root, g)
        layout = "v2" if is_directory_with_unified_xdf_files(g) else "v1"
        res = check_group(gdir)

        # Split absent inputs into genuinely-missing (rsync may still be running)
        # vs confirmed-permanent holes (verified absent at the source on gauss).
        missing = []
        known = []
        for label, (ok, detail) in res.items():
            if ok:
                continue
            if (g, label) in KNOWN_MISSING:
                known.append(f"{label} ({KNOWN_MISSING[(g, label)]})")
            else:
                missing.append(f"{label} ({detail})")

        if missing:
            note = "; ".join(missing)
            if known:
                note += "  [known holes: " + "; ".join(known) + "]"
            print(f"{g:<22} {layout:<6} {'MISSING':<8} {note}")
        elif known:
            holes_only += 1
            print(f"{g:<22} {layout:<6} {'OK*':<8} known holes: " + "; ".join(known))
        else:
            ready += 1
            print(f"{g:<22} {layout:<6} {'READY':<8}")

    accounted = ready + holes_only
    print(
        f"\n{accounted}/{len(groups)} group sessions have all SYNCABLE task inputs present "
        f"({ready} fully READY, {holes_only} OK* with only known/permanent holes)."
    )
    if accounted == len(groups):
        print("All task data that can sync is present. Safe to build task tables.")
    else:
        print(
            f"{len(groups) - accounted} group session(s) still MISSING inputs that exist "
            "on gauss — rsync may still be in progress."
        )
    # Exit 0 once nothing syncable is outstanding (known holes don't block).
    return 0 if accounted == len(groups) else 1


if __name__ == "__main__":
    sys.exit(main())
