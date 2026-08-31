"""FastAPI application: the public, Postgres-backed ToMCAT dataset interface.

Routes mirror the old Datasette URL space so existing links survive:

    /                         home / dataset landing page
    /<page>                   curated static pages (ethical-review, schema, ...)
    /tomcat                   database index (tables + estimated row counts)
    /tomcat/<table>           faceted browse (+ ?_format=csv|json export)
    /tomcat/-/query           read-only SQL console (?sql=... [&_format=csv|json])
    /-/structured_metadata.json   schema.org Dataset metadata
    /assets/...               static files (CSS, ERD diagram, consent PDFs)

Templates live in the package's templates/ (our overrides) with the project-root
templates/ as a fallback so the curated `pages/*` reuse unchanged. Postgres access
is always through the read-only tomcat_public engine in db.py.
"""

from __future__ import annotations

import csv
import io
import json
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import urlencode

import markdown as md
import yaml
from fastapi import FastAPI, HTTPException, Request
from fastapi.responses import (
    FileResponse,
    HTMLResponse,
    JSONResponse,
    StreamingResponse,
)
from fastapi.staticfiles import StaticFiles
from jinja2 import ChoiceLoader, Environment, FileSystemLoader, select_autoescape
from markupsafe import Markup

from dataset_website.settings import settings
from dataset_website import browse as browse_engine
from dataset_website import facets as facet_engine
from dataset_website import query as query_engine
from dataset_website.config import EXPORT_ROW_CAP
from dataset_website.message_specs import get_topic_index, topic_tree
from dataset_website.schema import Table, estimated_row_count, get_database

# --- Paths ------------------------------------------------------------------
PACKAGE_DIR = Path(__file__).resolve().parent  # .../dataset_website/dataset_website
PROJECT_ROOT = PACKAGE_DIR.parent  # project root (holds static/, templates/)
WEBAPP_TEMPLATES = PACKAGE_DIR / "templates"
ROOT_TEMPLATES = PROJECT_ROOT / "templates"
STATIC_DIR = PROJECT_ROOT / "static"
METADATA_PATH = PROJECT_ROOT / "metadata.yml"

DB_NAME = settings.db_name
DB_PREFIX = f"/{DB_NAME}"

ARTIFACT_DIR = Path(settings.artifact_dir)
# The only files the /downloads endpoint will serve. Bulk artifacts are generated
# offline (see Makefile: to_sqlite, pg_dump_artifact) and served as static files;
# they never touch the database connection.
BULK_ARTIFACTS = {
    "tomcat.db": "SQLite database — opens in pandas, R, DuckDB, the sqlite3 CLI.",
    "tomcat.dump": "PostgreSQL dump (custom format) — restore with pg_restore.",
}

# --- Site-level chrome (NOT schema): title, citations, schema.org, CSS links -
# Table/column docs come from Postgres COMMENTs; this is just page furniture.
with open(METADATA_PATH) as _f:
    SITE_METADATA = yaml.safe_load(_f) or {}

# --- Jinja environment ------------------------------------------------------
_jinja = Environment(
    loader=ChoiceLoader(
        [FileSystemLoader(str(WEBAPP_TEMPLATES)), FileSystemLoader(str(ROOT_TEMPLATES))]
    ),
    autoescape=select_autoescape(["html", "xml"]),
)


def _render_markdown(text: str) -> Markup:
    """Provide the render_markdown() the curated page templates expect."""
    return Markup(md.markdown(text, extensions=["fenced_code", "tables"]))


def _pretty_json(value) -> str:
    """Indent a JSON value for display. JSON columns arrive already deserialized to
    Python objects; a string is parsed first (and left untouched if it isn't JSON).
    Output is escaped by Jinja autoescape, then colorized client-side."""
    if value is None:
        return ""
    if isinstance(value, (dict, list)):
        return json.dumps(value, indent=2, ensure_ascii=False)
    if isinstance(value, str):
        try:
            return json.dumps(json.loads(value), indent=2, ensure_ascii=False)
        except (ValueError, TypeError):
            return value
    return str(value)


_jinja.globals["render_markdown"] = _render_markdown
_jinja.globals["pretty_json"] = _pretty_json
# Site metadata + the extra CSS URLs the old base template iterated over.
_jinja.globals["metadata"] = SITE_METADATA
_jinja.globals["extra_css_urls"] = SITE_METADATA.get("extra_css_urls", [])
_jinja.globals["extra_js_urls"] = SITE_METADATA.get("extra_js_urls", [])
_jinja.globals["db_name"] = DB_NAME


def render(template: str, request: Request, **context) -> HTMLResponse:
    ctx = {"request": request, **context}
    return HTMLResponse(_jinja.get_template(template).render(**ctx))


app = FastAPI(
    title=SITE_METADATA.get("title", "ToMCAT Dataset"), docs_url=None, redoc_url=None
)
app.mount("/assets", StaticFiles(directory=str(STATIC_DIR)), name="assets")


def _get_table_or_404(table_name: str) -> Table:
    db = get_database()
    table = db.tables.get(table_name)
    if table is None:
        raise HTTPException(status_code=404, detail=f"No such table: {table_name}")
    return table


# --- Export helpers ---------------------------------------------------------
def _csv_response(
    columns: list[str], rows: list[dict], filename: str
) -> StreamingResponse:
    def generate():
        buf = io.StringIO()
        writer = csv.DictWriter(buf, fieldnames=columns, extrasaction="ignore")
        writer.writeheader()
        yield buf.getvalue()
        buf.seek(0), buf.truncate(0)
        for row in rows:
            writer.writerow(row)
            yield buf.getvalue()
            buf.seek(0), buf.truncate(0)

    return StreamingResponse(
        generate(),
        media_type="text/csv",
        headers={"Content-Disposition": f'attachment; filename="{filename}.csv"'},
    )


def _json_response(columns: list[str], rows: list[dict]) -> JSONResponse:
    return JSONResponse({"columns": columns, "rows": rows})


class _TableLinks:
    """Builds browse URLs (filters, sort, paging, export) preserving current state.

    Keeps the compact Datasette-style query grammar (`col__op=value`) so links stay
    shareable and bookmarkable. Changing a filter or sort resets paging.
    """

    def __init__(self, path: str, params: list[tuple[str, str]]):
        self.path = path
        self.params = params

    def _qs(self, params: list[tuple[str, str]]) -> str:
        return f"?{urlencode(params)}" if params else ""

    def _set(self, key: str, value, drop: tuple[str, ...] = ()) -> str:
        drops = set(drop) | {key, "_offset"}
        kept = [(k, v) for (k, v) in self.params if k not in drops]
        kept.append((key, str(value)))
        return self.path + self._qs(kept)

    def filter(self, column: str, op: str, value) -> str:
        key = column if op == "exact" else f"{column}__{op}"
        kept = [(k, v) for (k, v) in self.params if k != "_offset"]
        kept.append((key, str(value)))
        return self.path + self._qs(kept)

    def remove_filter(self, f) -> str:
        key = f.column if f.op == "exact" else f"{f.column}__{f.op}"
        return self.path + self._qs([(k, v) for (k, v) in self.params if k != key])

    def sort_asc(self, column: str) -> str:
        return self._set("_sort", column, drop=("_sort_desc",))

    def sort_desc(self, column: str) -> str:
        return self._set("_sort_desc", column, drop=("_sort",))

    def page(self, offset: int) -> str:
        return self._set("_offset", max(offset, 0))

    def export(self, fmt: str) -> str:
        return self._set("_format", fmt)


# --- Routes -----------------------------------------------------------------
@app.get("/", response_class=HTMLResponse)
def home(request: Request):
    db = get_database()
    tables = [
        {
            "name": t.name,
            "description": t.description,
            "rows": estimated_row_count(t.name),
        }
        for t in db.tables.values()
    ]
    return render("index.html", request, tables=tables, database=DB_NAME)


@app.get("/-/structured_metadata.json")
def structured_metadata():
    return JSONResponse(SITE_METADATA.get("structured_metadata", {}))


@app.get("/robots.txt", include_in_schema=False)
def robots():
    """Crawler policy.

    Declared ahead of the ``/{page}`` catch-all, which would otherwise try to
    render ``pages/robots.txt.html`` and 404. See ``static/robots.txt`` for the
    rationale: the blocked paths are expensive, not private.
    """
    return FileResponse(STATIC_DIR / "robots.txt", media_type="text/plain")


@app.get("/download", response_class=HTMLResponse)
def download_index(request: Request):
    """List the bulk-download artifacts that currently exist on disk."""
    artifacts = []
    for name, desc in BULK_ARTIFACTS.items():
        path = ARTIFACT_DIR / name
        if not path.exists():
            continue
        stat = path.stat()
        sha_file = path.with_suffix(path.suffix + ".sha256")
        checksum = ""
        if sha_file.exists():
            checksum = sha_file.read_text().split()[0]
        artifacts.append(
            {
                "name": name,
                "description": desc,
                "size_gb": round(stat.st_size / 1e9, 2),
                "updated": datetime.fromtimestamp(stat.st_mtime, timezone.utc).strftime(
                    "%Y-%m-%d %H:%M UTC"
                ),
                "checksum": checksum,
                "url": f"/downloads/{name}",
            }
        )
    return render("download.html", request, artifacts=artifacts)


@app.get("/downloads/{name}")
def download_file(name: str):
    """Serve a bulk artifact. In production Caddy should serve /downloads/* directly
    off disk (range requests, no Python in the path); this is the fallback."""
    if name not in BULK_ARTIFACTS:
        raise HTTPException(status_code=404, detail="Unknown download")
    path = ARTIFACT_DIR / name
    if not path.exists():
        raise HTTPException(status_code=404, detail="Artifact not generated yet")
    return FileResponse(str(path), filename=name, media_type="application/octet-stream")


@app.get(DB_PREFIX, response_class=HTMLResponse)
def database_index(request: Request):
    db = get_database()
    tables = [
        {
            "name": t.name,
            "description": t.description,
            "rows": estimated_row_count(t.name),
        }
        for t in db.tables.values()
    ]
    return render("database.html", request, database=DB_NAME, tables=tables)


@app.get(f"{DB_PREFIX}/-/schema.json")
def schema_json():
    """Table -> column-name map for the SQL editor's autocomplete (CodeMirror
    sql-hint). Served from the cached reflection in get_database(), so it costs no
    database round-trip. Public by construction: it exposes exactly the tables and
    columns tomcat_public may already SELECT."""
    db = get_database()
    return JSONResponse({t.name: t.column_names for t in db.tables.values()})


@app.get(f"{DB_PREFIX}/-/schema-graph.json")
def schema_graph_json():
    """Tables + foreign-key edges for the interactive ERD on the schema page.

    Served from the cached reflection in get_database() (no DB round-trip). Public
    by construction: only tables tomcat_public may SELECT are listed, and edges to
    non-public tables were already dropped during reflection."""
    db = get_database()
    tables = [
        {
            "name": t.name,
            "pk": t.primary_keys,
            "columns": [
                {"name": c.name, "pk": c.primary_key, "fk": c.foreign_key}
                for c in t.columns
            ],
        }
        for t in db.tables.values()
    ]
    edges = [
        {
            "source": t.name,
            "source_columns": fk.constrained_columns,
            "target": fk.referred_table,
            "target_columns": fk.referred_columns,
        }
        for t in db.tables.values()
        for fk in t.foreign_keys
    ]
    return JSONResponse({"tables": tables, "edges": edges})


@app.get(f"{DB_PREFIX}/-/query", response_class=HTMLResponse)
def sql_console(request: Request):
    sql = request.query_params.get("sql", "")
    fmt = request.query_params.get("_format")
    result = error = None
    if sql.strip():
        try:
            cap = (
                EXPORT_ROW_CAP
                if fmt in ("csv", "json")
                else query_engine.SQL_CONSOLE_ROW_CAP
            )
            result = query_engine.run_query(sql, cap=cap)
        except query_engine.InvalidQuery as exc:
            error = str(exc)

    if result is not None and fmt == "csv":
        return _csv_response(result.columns, result.rows, "query")
    if result is not None and fmt == "json":
        return _json_response(result.columns, result.rows)

    return render(
        "query.html", request, database=DB_NAME, sql=sql, result=result, error=error
    )


@app.get(f"{DB_PREFIX}/{{table_name}}", response_class=HTMLResponse)
def table_view(table_name: str, request: Request):
    table = _get_table_or_404(table_name)
    params = list(request.query_params.multi_items())
    fmt = request.query_params.get("_format")

    if fmt in ("csv", "json"):
        result = browse_engine.run_browse(table, params, limit_override=EXPORT_ROW_CAP)
        if fmt == "csv":
            return _csv_response(result.column_names, result.rows, table.name)
        return _json_response(result.column_names, result.rows)

    estimate = estimated_row_count(table.name)
    result = browse_engine.run_browse(table, params, estimated_total=estimate)
    facets = facet_engine.run_facets(table, params)

    path = f"{DB_PREFIX}/{table.name}"
    links = _TableLinks(path, params)
    # Attach a remove-URL to each applied filter for the "remove [x]" chips.
    applied = [
        {"filter": f, "remove_url": links.remove_filter(f)} for f in result.filters
    ]

    # For the testbed-message table, let each `topic` facet value deep-link to its
    # schema docs. Empty for every other table, so the facet markup is a no-op there.
    documented_topics: set[str] = set()
    if table.name == "minecraft_testbed_message":
        documented_topics = set(get_topic_index()["documented"].keys())

    return render(
        "table.html",
        request,
        database=DB_NAME,
        table=table,
        result=result,
        facets=facets,
        operators=browse_engine.OPERATORS,
        applied=applied,
        links=links,
        documented_topics=documented_topics,
    )


# --- Message-bus schema reference -------------------------------------------
# Documents the JSON in minecraft_testbed_message.message, per topic. Registered
# BEFORE the /{page} catch-all so bare /messages isn't swallowed by static_page;
# the per-topic route uses {topic:path} because topics contain slashes.
@app.get("/messages", response_class=HTMLResponse)
def messages_index(request: Request):
    index = get_topic_index()
    return render(
        "messages_index.html",
        request,
        tree=topic_tree(index["documented"].keys()),
        count=len(index["documented"]),
        undocumented=index["undocumented"],
        observed_known=index["observed_known"],
    )


@app.get("/messages/{topic:path}", response_class=HTMLResponse)
def message_topic(topic: str, request: Request):
    doc = get_topic_index()["documented"].get(topic)
    if doc is None:
        raise HTTPException(status_code=404, detail=f"No documented topic: {topic}")
    return render("message_topic.html", request, topic=topic, doc=doc)


# --- Curated static pages ---------------------------------------------------
# Reuse the project-root templates/pages/*.html unchanged (they only need
# render_markdown + our base template). A page is served if its template exists.
@app.get("/{page}", response_class=HTMLResponse)
def static_page(page: str, request: Request):
    return _render_page(f"pages/{page}.html", request)


@app.get("/updates/{date}", response_class=HTMLResponse)
def update_page(date: str, request: Request):
    return _render_page(f"pages/updates/{date}.html", request)


def _render_page(template_path: str, request: Request) -> HTMLResponse:
    try:
        template = _jinja.get_template(template_path)
    except Exception as exc:  # jinja2.TemplateNotFound
        raise HTTPException(status_code=404, detail="Page not found") from exc
    return HTMLResponse(template.render(request=request))
