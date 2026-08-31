"""Static configuration for the public web app.

Almost everything the app needs is discovered from Postgres itself (schema via
reflection, descriptions via COMMENTs, the public table set via SELECT privilege).
The one thing Postgres has no natural home for is which columns deserve the opt-in
facet treatment, so that small, deliberately hand-curated mapping lives here.
"""

from __future__ import annotations

# Opt-in faceting. ONLY columns listed here ever get the
# `SELECT col, COUNT(*) ... GROUP BY col` facet query, and only under the role's
# statement timeout. Keep these to LOW-CARDINALITY columns (a handful of distinct
# values) -- faceting a high-cardinality column like a timestamp or a float channel
# is exactly the expensive operation we are avoiding. A table not listed here shows
# no facets at all.
FACET_COLUMNS: dict[str, list[str]] = {
    "affective_task_event": ["group_session", "participant", "event_type"],
    "audio_vocalics": ["group_session", "station"],
    "data_validity": [
        "group_session",
        "participant",
        "station",
        "task",
        "modality",
        "is_valid",
    ],
    "eeg_device": ["group_session", "station", "device_id"],
    "eeg_raw": ["group_session", "station", "participant", "task"],
    # *_sync tables have no participant/task columns; PK is (group_session,
    # frequency, station, id). Facet on what actually exists.
    "eeg_sync": ["group_session", "station", "frequency"],
    "ekg_sync": ["group_session", "station", "frequency"],
    "finger_tapping_task_observation": ["group_session", "event_type"],
    "fnirs_raw": ["group_session", "station", "participant", "task"],
    "fnirs_sync": ["group_session", "station", "frequency"],
    "gaze_raw": ["group_session", "station", "participant", "task"],
    "gsr_sync": ["group_session", "station", "frequency"],
    "minecraft_mission": ["group_session", "name"],
    "minecraft_testbed_message": ["mission", "topic"],
    "ping_pong_competitive_task_observation": ["group_session"],
    "ping_pong_cooperative_task_observation": ["group_session"],
    "rest_state_task": ["group_session"],
    "screen_capture": ["group_session", "station", "participant", "task"],
}

# Maximum distinct values to show per facet before truncating.
FACET_SIZE = 30

# Default and maximum page sizes for table browse.
DEFAULT_PAGE_SIZE = 100
MAX_PAGE_SIZE = 1000

# Hard row cap applied to the arbitrary SQL console results page (export streams
# more). Keeps a `SELECT * FROM eeg_raw` from trying to render millions of rows.
SQL_CONSOLE_ROW_CAP = 1000

# Row cap for CSV/JSON export. The statement timeout is the real bound on huge
# tables; this caps memory. Full-table bulk access remains a download, not a query.
EXPORT_ROW_CAP = 50_000
