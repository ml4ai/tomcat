-- =============================================================================
-- setup_public_role.sql
--
-- Provision the powerless, read-only Postgres role the public web app connects
-- as. Idempotent; safe to re-run. Run AFTER bin/provision_postgres.sh (which
-- creates the tomcat_curators write group and the tomcat database).
--
-- IMPORTANT: tomcat_public is INDEPENDENT of the internal `tomcat_readers`
-- group. tomcat_readers is for lab members who connect as themselves over peer
-- auth; we deliberately do NOT make the public role a member of it, so the
-- hardening below (10s statement timeout, etc.) applies ONLY to the public web
-- app and never to lab members.
--
-- This file deliberately does NOT set the password (no secrets in git). Set it
-- out of band after running, e.g.:
--     psql -d tomcat -c "ALTER ROLE tomcat_public PASSWORD 'CHOOSE-A-STRONG-ONE'"
-- then put that value in the gitignored .env as WEB_DB_PASS. The app connects
-- over TCP/socket with scram-sha-256; ensure pg_hba.conf permits it for this role.
--
-- Usage (run as a Postgres superuser / tomcat_curators over local peer auth).
-- Use -1 so the whole file runs in ONE transaction: section 2 revokes before it
-- re-grants, and without -1 a live web app would see a moment with no SELECT.
--     psql -v ON_ERROR_STOP=1 -1 -d tomcat -f bin/setup_public_role.sql
-- =============================================================================

-- --- 1. The LOGIN role (idempotent) -----------------------------------------
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'tomcat_public') THEN
    -- LOGIN so the app can authenticate; no other attributes (no SUPERUSER,
    -- CREATEDB, CREATEROLE, BYPASSRLS). Password is set out of band (see header).
    CREATE ROLE tomcat_public LOGIN;
  END IF;
END
$$;

-- --- 2. Read-only access, granted DIRECTLY to tomcat_public -----------------
-- No group membership: tomcat_public stands alone, separate from tomcat_readers.
--
-- EXPOSURE MODEL (read this): the public web app shows EVERY table tomcat_public
-- can SELECT, so this list IS the public surface of the dataset. It is fail-
-- CLOSED -- a table is private unless it is named below. Publishing a new table
-- is therefore a deliberate edit to this file, reviewed like any other change.
--
-- This replaced a fail-OPEN model on 2026-08-21 (grant broadly, then revoke the
-- internal ones). Under that model any new internal, staging or intermediate
-- table appeared on the public site the moment the pipeline created it, and
-- stayed there until somebody remembered to add it to a revoke list. For a
-- database carrying identifiable participant data -- demographics, a health
-- block, screen captures -- the default has to be the other way round.
GRANT CONNECT ON DATABASE tomcat TO tomcat_public;
GRANT USAGE   ON SCHEMA public   TO tomcat_public;

-- The allowlist. Adding a name here publishes that table to the world.
DO $$
DECLARE
  t text;
  missing text[] := ARRAY[]::text[];
  extra text[] := ARRAY[]::text[];
  published text[] := ARRAY[
    'affective_task_event',
    'audio_vocalics',
    'data_validity',
    'eeg_device',
    'eeg_raw',
    'eeg_sync',
    'ekg_sync',
    'finger_tapping_task_observation',
    'fnirs_raw',
    'fnirs_sync',
    'gaze_raw',
    'group_session',
    'gsr_sync',
    'minecraft_mission',
    'minecraft_testbed_message',
    'modality',
    'participant',
    'ping_pong_competitive_task_observation',
    'ping_pong_cooperative_task_observation',
    'post_game_survey',
    'rest_state_task',
    'screen_capture',
    'station',
    'task'
  ];
BEGIN
  -- Start from nothing so the array above is authoritative rather than additive:
  -- removing a name here actually unpublishes the table on the next run.
  EXECUTE 'REVOKE SELECT ON ALL TABLES IN SCHEMA public FROM tomcat_public';

  FOREACH t IN ARRAY published LOOP
    IF EXISTS (
      SELECT FROM information_schema.tables
      WHERE table_schema = 'public' AND table_name = t
    ) THEN
      EXECUTE format('GRANT SELECT ON public.%I TO tomcat_public', t);
    ELSE
      missing := missing || t;
    END IF;
  END LOOP;

  -- Drift report. Neither case is an error -- the pipeline legitimately creates
  -- intermediates, and a table can be listed before it is built -- but both are
  -- worth seeing, because silence is how the old model went wrong.
  SELECT array_agg(table_name ORDER BY table_name) INTO extra
  FROM information_schema.tables
  WHERE table_schema = 'public' AND NOT (table_name = ANY (published));

  IF array_length(missing, 1) > 0 THEN
    RAISE NOTICE 'allowlisted but not present (not granted): %', array_to_string(missing, ', ');
  END IF;
  IF array_length(extra, 1) > 0 THEN
    RAISE NOTICE 'present but NOT published (correct if internal): %', array_to_string(extra, ', ');
  END IF;
END
$$;

-- Undo the fail-open default privilege. Without this, tables created later by
-- the curators group would still auto-grant SELECT to tomcat_public and defeat
-- the allowlist above. Idempotent: a no-op if it was never set.
ALTER DEFAULT PRIVILEGES FOR ROLE tomcat_curators IN SCHEMA public
  REVOKE SELECT ON TABLES FROM tomcat_public;

-- --- 3. Hardening (applies ONLY to tomcat_public's own sessions) ------------
-- Role-level session defaults; they do not affect any other role. They are
-- defense-in-depth on top of the SELECT-only grants above: the real boundary is
-- that tomcat_public can only SELECT, but these cap blast radius (no writes even
-- if a default is toggled, no runaway/idle sessions).
ALTER ROLE tomcat_public SET default_transaction_read_only = on;
ALTER ROLE tomcat_public SET statement_timeout = '10s';
ALTER ROLE tomcat_public SET idle_in_transaction_session_timeout = '15s';
ALTER ROLE tomcat_public SET lock_timeout = '2s';
-- Pin the schema search path so unqualified names always resolve to public.
ALTER ROLE tomcat_public SET search_path = public;

-- --- 4. Internal tables need no action ------------------------------------
-- Under the fail-closed model of section 2 there is nothing to hide: a table is
-- unreachable by tomcat_public unless it is on the allowlist. `fnirs_tmp` (the
-- intermediate built during the sync step) used to need an explicit REVOKE and
-- no longer does. Section 2's drift report will list it whenever it exists.
