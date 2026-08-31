"""Message-bus schema reference for the public web app.

The `minecraft_testbed_message.message` column holds a JSON payload whose shape
varies by `topic`. The authoritative definitions are the ASIST testbed message
specs, vendored under `message_specs/` (see message_specs/PROVENANCE.md). Each
topic maps to one JSON Schema (Draft-07) composed of a `header`/`msg`/`data`
object via relative `$ref`s, plus a paired Markdown file with prose + examples.

This module reads the vendored specs once at startup (cached, like
webapp/schema.py:get_database()), resolves the `$ref` graph into a normalized
per-topic model, and filters it to the topics actually present in the database so
the published reference only ever documents data a user can really query.

Nothing here touches user input or the writable database; the only DB access is a
single cached `SELECT DISTINCT topic` through the read-only public engine.
"""

from __future__ import annotations

import csv
import json
import re
from dataclasses import dataclass, field
from functools import lru_cache
from pathlib import Path

from sqlalchemy import text

from dataset_website.db import engine

# Vendored spec tree (resolved relative to this module, so it survives any cwd).
SPECS_DIR = Path(__file__).resolve().parent / "message_specs"
CSV_PATH = SPECS_DIR / "message_topics.csv"

# Stop recursing into deeply nested objects/arrays; the field table is a guide,
# not a substitute for the raw schema (which is always available via the toggle).
MAX_DEPTH = 4

# Boilerplate field titles ("The Sender Schema", "Unique Block Id Schema", ...)
# carry no information; only keep a title that isn't this auto-generated pattern.
_BOILERPLATE_TITLE = re.compile(r"schema\s*$", re.IGNORECASE)

# Fenced code blocks in the paired .md, used to surface example messages.
_FENCE = re.compile(r"```(?:json)?\s*\n(.*?)```", re.DOTALL)


@dataclass
class Field:
    """One row of a rendered field table (header / msg / data)."""

    name: str  # dotted path, e.g. "explanation" or "items[].x"
    json_type: str  # "string", "string|null", "array<object>", ...
    required: bool
    enum: list | None  # allowed values (from `enum`, or `[const]`)
    description: str
    depth: int  # nesting level, for indentation in the template


@dataclass
class Section:
    """A named group of fields. Standard messages have header/msg/data; some
    topics are flat (their payload sits at the top level) -- captured as `message`."""

    title: str
    fields: list[Field]


@dataclass
class TopicDoc:
    topic: str  # spec topic from the CSV (may contain MQTT wildcards)
    description: str  # CSV description (may be "")
    schema_path: str  # relative path of the schema file, for display
    sections: list[Section] = field(default_factory=list)
    examples: list[str] = field(default_factory=list)  # raw JSON example blocks
    prose: str = ""  # full paired-.md markdown, shown collapsibly
    raw_schema: dict = field(default_factory=dict)  # fully resolved, for the toggle


# --- Schema loading + $ref resolution --------------------------------------
@lru_cache(maxsize=None)
def _load_json(path: Path) -> dict | None:
    """Parse a vendored schema, tolerating the trailing commas a few of them carry.

    Returns None for a file that still won't parse, so one malformed spec degrades
    that single topic instead of breaking the whole reference.
    """
    try:
        raw = path.read_text()
    except OSError:  # ref points outside the vendored tree / missing file
        return None
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        cleaned = re.sub(r",(\s*[}\]])", r"\1", raw)  # drop trailing commas, retry
        try:
            return json.loads(cleaned)
        except json.JSONDecodeError:
            return None


def _deep_merge(base: dict, over: dict) -> dict:
    """Overlay `over` onto `base`; recurse where both sides are dicts.

    Used to apply a schema's local property overrides on top of a `$ref` target
    (e.g. a `message_type` `const` layered over Common_Header's `enum`).
    """
    out = dict(base)
    for key, value in over.items():
        if isinstance(value, dict) and isinstance(out.get(key), dict):
            out[key] = _deep_merge(out[key], value)
        else:
            out[key] = value
    return out


def _json_pointer(root: dict, ref: str):
    """Resolve an in-document `#/a/b` JSON pointer against `root`; None if absent."""
    out = root
    for seg in ref.lstrip("#").split("/"):
        if seg == "":
            continue
        seg = seg.replace("~1", "/").replace("~0", "~")
        if isinstance(out, dict) and seg in out:
            out = out[seg]
        else:
            return None
    return out


def _resolve(node, base_dir: Path, root: dict, seen: frozenset, depth: int = 0):
    """Recursively inline every `$ref`. File refs (`../X.json`) resolve against the
    *referring* file's directory; in-document refs (`#/definitions/X`) resolve
    against that file's `root`. `seen` holds the ref identities on the current chain
    for cycle detection (diamonds are fine; only a ref re-entering its own chain is
    a cycle). `depth` is a hard backstop against any pathological nesting."""
    if depth > 60:
        return node
    if isinstance(node, list):
        return [_resolve(item, base_dir, root, seen, depth + 1) for item in node]
    if not isinstance(node, dict):
        return node

    if "$ref" in node:
        ref = str(node["$ref"]).strip()  # some refs carry trailing whitespace
        siblings = {k: v for k, v in node.items() if k != "$ref"}
        resolved_siblings = _resolve(siblings, base_dir, root, seen, depth + 1)

        if ref.startswith("#"):  # in-document pointer
            ident = (id(root), ref)
            target = None if ident in seen else _json_pointer(root, ref)
            next_root, next_dir, next_seen = root, base_dir, seen | {ident}
        else:  # external file ref
            ref_path = (base_dir / ref).resolve()
            ident = str(ref_path)
            target = None if ident in seen else _load_json(ref_path)
            next_root, next_dir, next_seen = target, ref_path.parent, seen | {ident}

        if not isinstance(target, dict):
            # Cycle, dangling, unparseable, or non-object ref: keep local overrides.
            return {**resolved_siblings, "$ref_unresolved": ref}
        resolved_target = _resolve(target, next_dir, next_root, next_seen, depth + 1)
        base = resolved_target if isinstance(resolved_target, dict) else {}
        return _deep_merge(base, resolved_siblings)

    return {k: _resolve(v, base_dir, root, seen, depth + 1) for k, v in node.items()}


# --- Flattening a JSON-Schema `properties` map into field rows ---------------
def _type_label(prop: dict) -> str:
    t = prop.get("type")
    if isinstance(t, list):
        return "|".join(str(x) for x in t)
    if t == "array":
        items = prop.get("items")
        inner = ""
        if isinstance(items, dict):
            it = items.get("type")
            inner = "|".join(it) if isinstance(it, list) else (it or "object")
        return f"array<{inner}>" if inner else "array"
    if t:
        return str(t)
    if "const" in prop:
        return type(prop["const"]).__name__
    if isinstance(prop.get("properties"), dict):
        return "object"
    return ""


def _clean_desc(prop: dict) -> str:
    desc = prop.get("description")
    if desc:
        return str(desc).strip()
    title = (prop.get("title") or "").strip()
    return "" if _BOILERPLATE_TITLE.search(title) else title


def _flatten(properties: dict, required: set, depth: int, prefix: str) -> list[Field]:
    fields: list[Field] = []
    if not isinstance(properties, dict):
        return fields
    for name, prop in properties.items():
        if not isinstance(prop, dict):
            continue
        full = prefix + name
        fields.append(
            Field(
                name=full,
                json_type=_type_label(prop),
                required=name in required,
                enum=[prop["const"]] if "const" in prop else prop.get("enum"),
                description=_clean_desc(prop),
                depth=depth,
            )
        )
        # Recurse into nested objects and arrays-of-objects, up to MAX_DEPTH.
        nested, nested_prefix = None, ""
        if prop.get("type") == "object" and isinstance(prop.get("properties"), dict):
            nested, nested_prefix = prop, full + "."
        elif prop.get("type") == "array" and isinstance(prop.get("items"), dict):
            items = prop["items"]
            if isinstance(items.get("properties"), dict):
                nested, nested_prefix = items, full + "[]."
        if nested is not None:
            if depth + 1 < MAX_DEPTH:
                fields += _flatten(
                    nested["properties"],
                    set(nested.get("required", [])),
                    depth + 1,
                    nested_prefix,
                )
            else:
                fields.append(
                    Field(nested_prefix + "…", "…", False, None, "", depth + 1)
                )
    return fields


_KNOWN_SECTIONS = ("header", "msg", "data")


def _build_sections(schema: dict) -> list[Section]:
    """Turn a resolved message schema into ordered field sections.

    Most topics nest their content under header/msg/data; others put the payload
    straight at the top level (metadata.*, control responses, status messages).
    The standard sections come first, then any remaining top-level fields are
    grouped under `message`, so no topic's real fields are dropped.
    """
    props = schema.get("properties", {})
    if not isinstance(props, dict):
        return []
    sections: list[Section] = []
    for key in _KNOWN_SECTIONS:
        node = props.get(key)
        if isinstance(node, dict) and isinstance(node.get("properties"), dict):
            fields = _flatten(node["properties"], set(node.get("required", [])), 0, "")
            if fields:
                sections.append(Section(key, fields))
    leftover = {k: v for k, v in props.items() if k not in _KNOWN_SECTIONS}
    if leftover:
        fields = _flatten(leftover, set(schema.get("required", [])), 0, "")
        if fields:
            sections.append(Section("message", fields))
    return sections


# --- Public spec model -------------------------------------------------------
def _read_csv() -> list[tuple[str, str, str]]:
    rows: list[tuple[str, str, str]] = []
    with open(CSV_PATH, newline="") as f:
        reader = csv.reader(f)
        next(reader, None)  # header
        for row in reader:
            if not row or not row[0].strip():
                continue
            topic = row[0].strip()
            schema = row[1].strip() if len(row) > 1 else ""
            desc = row[2].strip() if len(row) > 2 else ""
            rows.append((topic, schema, desc))
    return rows


def _build_topic_doc(topic: str, schema_rel: str, desc: str) -> TopicDoc | None:
    schema_path = (SPECS_DIR / schema_rel).resolve()
    if not schema_rel or not schema_path.exists():
        return None
    loaded = _load_json(schema_path)
    if loaded is None:  # malformed top-level schema -- can't document this topic
        return None
    resolved = _resolve(loaded, schema_path.parent, loaded, frozenset())

    md_path = schema_path.with_suffix(".md")
    prose, examples = "", []
    if md_path.exists():
        prose = md_path.read_text()
        examples = [
            m.strip() for m in _FENCE.findall(prose) if m.strip().startswith("{")
        ]

    return TopicDoc(
        topic=topic,
        description=desc,
        schema_path=schema_rel,
        sections=_build_sections(resolved),
        examples=examples,
        prose=prose,
        raw_schema=resolved,
    )


@lru_cache(maxsize=1)
def get_topic_docs() -> dict[str, TopicDoc]:
    """All spec topics -> normalized doc, keyed by the raw CSV topic string."""
    docs: dict[str, TopicDoc] = {}
    for topic, schema_rel, desc in _read_csv():
        doc = _build_topic_doc(topic, schema_rel, desc)
        if doc is not None:
            docs[topic] = doc
    return docs


# --- Restricting the reference to topics present in the data -----------------
def _topic_matches(spec: str, observed: str) -> bool:
    """MQTT topic match: `+` is one level, `#` is the rest. Spec topics may use
    wildcards (e.g. `agent/prediction/+`); DB topics are always concrete."""
    s, o = spec.split("/"), observed.split("/")
    if "#" in s:
        s = s[: s.index("#")]
        return o[: len(s)] == s
    if len(s) != len(o):
        return False
    return all(a == "+" or a == b for a, b in zip(s, o))


@lru_cache(maxsize=1)
def get_observed_topics() -> frozenset[str] | None:
    """Distinct `topic` values in minecraft_testbed_message, or None if the query
    can't run (e.g. times out / table missing) -- callers then fall back to the
    full spec rather than showing an empty reference."""
    try:
        with engine.connect() as conn:
            rows = (
                conn.execute(
                    text("SELECT DISTINCT topic FROM minecraft_testbed_message")
                )
                .scalars()
                .all()
            )
        return frozenset(r for r in rows if r)
    except Exception:
        return None


def topic_tree(topics) -> list[dict]:
    """Nest a flat list of `a/b/c` topics into a sorted tree for the index page.

    Each node is {name, topic, children}; `topic` is set only on nodes that are
    themselves a documented topic (so an intermediate path with no leaf is just a
    grouping label).
    """
    root: dict = {}
    for t in sorted(topics):
        children = root
        parts = t.split("/")
        for i, part in enumerate(parts):
            node = children.setdefault(part, {"name": part, "topic": None, "kids": {}})
            if i == len(parts) - 1:
                node["topic"] = t
            children = node["kids"]

    def to_list(d: dict) -> list[dict]:
        return [
            {"name": n["name"], "topic": n["topic"], "children": to_list(n["kids"])}
            for name, n in sorted(d.items())
        ]

    return to_list(root)


@lru_cache(maxsize=1)
def get_topic_index() -> dict:
    """Resolve which topics to document.

    Returns:
      documented:   {concrete_topic -> TopicDoc} for observed topics that match a
                    spec (keyed by the real DB topic so per-topic links use it);
      undocumented: observed topics with no matching spec (surfaced, never hidden);
      observed_known: False when the DISTINCT query failed and we fell back to the
                    full spec set.
    """
    docs = get_topic_docs()
    observed = get_observed_topics()

    if observed is None:
        # Degrade to the full spec reference rather than an empty page.
        return {
            "documented": dict(docs),
            "undocumented": [],
            "observed_known": False,
        }

    documented: dict[str, TopicDoc] = {}
    for obs in observed:
        if obs in docs:  # exact match wins over any wildcard
            documented[obs] = docs[obs]
            continue
        for spec, doc in docs.items():
            if ("+" in spec or "#" in spec) and _topic_matches(spec, obs):
                documented[obs] = doc
                break

    undocumented = sorted(o for o in observed if o not in documented)
    return {
        "documented": documented,
        "undocumented": undocumented,
        "observed_known": True,
    }
