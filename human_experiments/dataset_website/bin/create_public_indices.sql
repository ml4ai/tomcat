-- =============================================================================
-- create_public_indices.sql
--
-- Indexes that speed up the public web app's browse + facet queries. Idempotent
-- (CREATE INDEX IF NOT EXISTS). Run as a tomcat_curators member AFTER the data is
-- loaded/synced:
--     psql -v ON_ERROR_STOP=1 -d tomcat -f bin/create_public_indices.sql
--
-- Rationale per table:
--  * Raw signal tables (eeg_raw, fnirs_raw, gaze_raw) already get a
--    (group_session, station, task) index from bin/create_table_indices.py, and
--    their PK (group_session, station, participant, id) serves group/station/
--    participant filtering. Nothing more needed here.
--  * The *_sync tables have PK (group_session, frequency, station, id) -- so
--    `station` is NOT a usable index prefix unless `frequency` is also fixed.
--    Browsing/faceting by station is common, so add (group_session, station).
--  * audio_vocalics PK (group_session, station, ...) already covers its facets.
--
-- After creating indexes, run ANALYZE so the planner's row estimates (used for the
-- table index page's fast counts) are fresh.
-- =============================================================================

CREATE INDEX IF NOT EXISTS idx_eeg_sync_gs_station   ON eeg_sync   (group_session, station);
CREATE INDEX IF NOT EXISTS idx_ekg_sync_gs_station   ON ekg_sync   (group_session, station);
CREATE INDEX IF NOT EXISTS idx_gsr_sync_gs_station   ON gsr_sync   (group_session, station);
CREATE INDEX IF NOT EXISTS idx_fnirs_sync_gs_station ON fnirs_sync (group_session, station);

ANALYZE eeg_sync;
ANALYZE ekg_sync;
ANALYZE gsr_sync;
ANALYZE fnirs_sync;
