# Provenance

This directory holds the ASIST Minecraft testbed message specifications used by the
webapp's Message Bus Reference (`/messages`, see `../message_specs.py`), which reads
`message_topics.csv` and the JSON Schemas here to document the JSON payloads in the
`minecraft_testbed_message.message` column.

It began as a copy of an upstream tree but is now a **local fork**: this copy is the
source of truth for this project. Edit the schemas here freely; record substantive
changes under "Local changes" below. (It is excluded from ruff in `pyproject.toml`,
so the tooling won't reformat it.)

## Origin

- Forked from `ml4ai/minecraft_testbed` (`git@github.com:ml4ai/minecraft_testbed.git`),
  subdirectory `MessageSpecs/`, at commit `1cc7178d1fb61081e159638892e2ef9021c92045`.
- Upstream lineage: https://gitlab.com/artificialsocialintelligence/study3/-/tree/main/MessageSpecs

## What was copied

`message_topics.csv`, all `*.json` schemas, and the paired `*.md` docs. The prebuilt
viewer assets (`event_message.html`, `schema_doc.css`, `schema_doc.min.js`) were not
copied.

## Pulling later upstream changes

Because this is a fork, re-importing upstream is a **manual merge**, not a clobber:
diff the upstream tree against this one and apply wanted changes, preserving the local
edits logged below. The parser also tolerates malformed JSON per-file (trailing-comma
strip + graceful degrade), so an upstream file that won't parse only affects its own
topic.

## Local changes

(Log edits made to the forked schemas here, so a future merge can preserve them.)

- Fixed invalid JSON (removed trailing commas) so the affected topics render fully
  instead of degrading:
  - `Agent/Prediction/State/agent_state_prediction_message.json`
  - `LocationMonitor/connection.json`
- `AC_UAZ_TA1_ASR_Agent/example_asr_message.json` also has malformed JSON, but it is
  an unused example (no topic or schema references it — the ASR topics use
  `asr_message.json`), so it was left as-is rather than reconstructed.
