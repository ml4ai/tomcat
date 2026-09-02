# The ToMCAT Dataset — Zenodo record

This record is the citable, persistent identifier for the ToMCAT dataset. The
dataset's living home, with every table browsable and queryable, the full
PostgreSQL dump, documentation, errata and updates, is

    https://tomcat.lab.pyarelal.xyz

(formerly https://tomcat.ivilab.org, the address printed in the paper).

## What is in this record

`tomcat-core.db` — one SQLite file with every table of the dataset **except**
the seven high-rate signal tables. That is:

| Table | Rows | What |
|---|---|---|
| `group_session` | 40 | One row per experiment session |
| `participant` | 114 | Demographics and self-report survey responses |
| `station` | 4 | The lion / tiger / leopard / cheetah workstations |
| `task` | 9 | Task identifiers |
| `modality` | 3 | Sensor modalities used in `data_validity` |
| `data_validity` | 3,060 | Which participant × task × modality recordings are valid |
| `eeg_device` | 120 | Station-to-EEG-amplifier mapping per session |
| `rest_state_task` | 40 | Rest-state period start/stop times |
| `affective_task_event` | 14,586 | Individual and team affective task events |
| `finger_tapping_task_observation` | 99,875 | Finger-tapping task observations |
| `ping_pong_competitive_task_observation` | 647,242 | Competitive ping-pong frames |
| `ping_pong_cooperative_task_observation` | 323,806 | Cooperative ping-pong frames |
| `post_game_survey` | 96 | Post-mission survey responses |
| `minecraft_mission` | 100 | One row per Minecraft mission (trial) |
| `minecraft_testbed_message` | 6,369,537 | Every testbed message-bus message, as JSON |
| `fnirs_raw` | 6,849,295 | Raw fNIRS at the device rate |
| `screen_capture` | 4,104,815 | Screen-capture frame references (URLs) |
| `_schema_docs` | 268 | Table and column descriptions |

Row counts are exact `count(*)` values from the export of 2026-09-02, checked
against the PostgreSQL source inside the same snapshot.

**Not in this record:** `eeg_raw`, `gaze_raw`, `fnirs_sync`, `eeg_sync`,
`audio_vocalics`, `gsr_sync`, `ekg_sync`. Together they are about 400 GB of the
413 GB database; SQLite does not compress, and a file that size is neither
downloadable nor usable in the tools this file is for. They are available on
the site one session at a time, and in full in the PostgreSQL dump at
https://tomcat.lab.pyarelal.xyz/download.

## Using the file

- `sqlite3 tomcat-core.db`
- pandas: `pd.read_sql("select * from participant", sqlite3.connect("tomcat-core.db"))`
- R: `DBI::dbConnect(RSQLite::SQLite(), "tomcat-core.db")`
- DuckDB: `ATTACH 'tomcat-core.db' (TYPE sqlite);`

Schema notes:

- Primary keys are present as `UNIQUE` indexes with the same names as in
  PostgreSQL (`<table>_pkey`), plus the secondary indexes the site uses.
- Booleans are stored as integers 0/1. `NULL` is preserved as `NULL`; an empty
  string is an empty string.
- `minecraft_testbed_message.message` is JSON text. SQLite's JSON functions
  work on it directly: `json_extract(message, '$.data.text')`. The message
  formats are documented at https://tomcat.lab.pyarelal.xyz/messages.
- Timestamps are stored twice, as `timestamp_unix` (text, seconds with
  fractional part) and `timestamp_iso8601`, exactly as in PostgreSQL.
- `select * from _schema_docs where table_name = 'participant'` gives the
  column descriptions.

## Integrity

The record includes `tomcat-core.db.sha256`. Verify with
`sha256sum -c tomcat-core.db.sha256`.

## Citation

Pyarelal, A., Duong, E., Shibu, C. J., Soares, P., Boyd, S., Khosla, P.,
Pfeifer, V., Zhang, D., Andrews, E. S., Champlin, R., Raymond, V. P.,
Krishnaswamy, M., Morrison, C., Butler, E., & Barnard, K. (2023). The ToMCAT
Dataset. *Thirty-seventh Conference on Neural Information Processing Systems
Datasets and Benchmarks Track.* https://openreview.net/forum?id=ZJWQfgXQb6

Please cite both the paper and this record's DOI.

## License

CC BY-NC-SA 4.0. https://creativecommons.org/licenses/by-nc-sa/4.0/
