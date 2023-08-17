#!/usr/bin/env python

import logging
import os
import sqlite3
import sys
from logging import info, error

import pyxdf
from tqdm import tqdm

from config import DB_PATH, USER
from utils import (
    cd,
    should_ignore_directory,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
)

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/build_gaze_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def recreate_gaze_table():
    info("Processing directories...")

    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        # Foreign key constraints are disabled by default (for backwards compatibility), so must be enabled separately f
        # or each database connection (https://www.sqlite.org/foreignkeys.html).
        db_connection.execute("PRAGMA foreign_keys = 1;")
        info("Dropping gaze table")
        db_connection.execute("DROP TABLE IF EXISTS gaze_raw")
        db_connection.execute(
            """
            CREATE TABLE gaze_raw (
                group_session TEXT NOT NULL,
                task TEXT,
                station TEXT NOT NULL,
                participant TEXT NOT NULL,
                timestamp_unix TEXT NOT NULL,
                timestamp_iso8601 TEXT NOT NULL,
                confidence REAL NOT NULL,
                norm_pos_x REAL NOT NULL,
                norm_pos_y REAL NOT NULL,
                gaze_point_3d_x REAL NOT NULL,
                gaze_point_3d_y REAL NOT NULL,
                gaze_point_3d_z REAL NOT NULL,
                eye_center0_3d_x REAL NOT NULL,
                eye_center0_3d_y REAL NOT NULL,
                eye_center0_3d_z REAL NOT NULL,
                eye_center1_3d_x REAL NOT NULL,
                eye_center1_3d_y REAL NOT NULL,
                eye_center1_3d_z REAL NOT NULL,
                gaze_normal0_x REAL NOT NULL,
                gaze_normal0_y REAL NOT NULL,
                gaze_normal0_z REAL NOT NULL,
                gaze_normal1_x REAL NOT NULL,
                gaze_normal1_y REAL NOT NULL,
                gaze_normal1_z REAL NOT NULL,
                diameter0_2d REAL NOT NULL,
                diameter1_2d REAL NOT NULL,
                diameter0_3d REAL NOT NULL,
                diameter1_3d REAL NOT NULL,
                FOREIGN KEY(group_session) REFERENCES group_session(id)
                FOREIGN KEY(task) REFERENCES task(id)
                FOREIGN KEY(station) REFERENCES station(id)
                FOREIGN KEY(participant) REFERENCES participant(id)
            );"""
        )


def insert_raw_unlabeled_data():
    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        db_connection = sqlite3.connect(DB_PATH)
        with db_connection:
            db_connection.execute("PRAGMA foreign_keys = 1;")
            for session in tqdm(
                sorted(directories_to_process), unit="directories"
            ):
                if not is_directory_with_unified_xdf_files(session):
                    process_directory_v1(session, db_connection)
                else:
                    process_directory_v2(session, db_connection)


def insert_data_into_table(stream, session, station, db_connection):
    # We insert a participant ID of -1 since we don't actually know for sure
    # who the participant is - we will need to consult the data validity table
    # to learn the ID, since the originally scheduled participant might be
    # replaced by an experimenter partway through the group session.
    task = None
    participant_id = -1
    data = [
        [
            session,
            task,
            station,
            participant_id,
            timestamp,
            convert_unix_timestamp_to_iso8601(timestamp),
            *list(map(str, stream["time_series"][i])),
        ]
        for i, timestamp in enumerate(stream["time_stamps"])
    ]

    with db_connection:
        query = (
            "INSERT into gaze_raw VALUES(?, ?, ?, ?, ?, ?,"
            + ",".join(
                [
                    "?"
                    for _ in stream["info"]["desc"][0]["channels"][0][
                        "channel"
                    ]
                ]
            )
            + ")"
        )
        db_connection.executemany(query, data)


def process_directory_v1(session, db_connection):
    info(f"Processing directory {session}")
    with cd(f"{session}"):
        for station in ("lion", "tiger", "leopard"):
            xdf_file = (
                f"{station}/eeg_fnirs_pupil/{station}_eeg_fnirs_pupil.xdf"
            )
            try:
                streams, header = pyxdf.load_xdf(
                    xdf_file, select_streams=[{"type": "Gaze"}]
                )
            except ValueError as e:
                error(f"[MISSING DATA]: No Gaze stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            stream = streams[0]
            insert_data_into_table(stream, session, station, db_connection)


def process_directory_v2(session, db_connection):
    """Process directory assuming unified XDF files."""
    info(f"Processing directory {session}")
    with cd(f"{session}/lsl"):
        for xdf_file in ("block_1.xdf", "block_2.xdf"):
            try:
                streams, header = pyxdf.load_xdf(
                    xdf_file, select_streams=[{"type": "Gaze"}]
                )
            except ValueError as e:
                error(f"[MISSING DATA]: No Gaze stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            for stream in streams:
                station = stream["info"]["info"]["hostname"][0]
                insert_data_into_table(stream, session, station, db_connection)


def create_indices():
    """Create indices for efficient querying"""
    info("Creating index for Gaze table.")
    db_connection = sqlite3.connect(DB_PATH)

    with db_connection:
        db_connection.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_group_session_station
            ON gaze_raw (group_session, station);
        """
        )

        db_connection.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_timestamp_unix
            ON gaze_raw (timestamp_unix);
        """
        )

        db_connection.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_participant
            ON gaze_raw (participant);
        """
        )

        db_connection.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_group_session
            ON gaze_raw (group_session);
        """
        )

        db_connection.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_task
            ON gaze_raw (task);
        """
        )


def label_rest_state_task_data(
    group_session, station, participant_id, db_connection
):
    with db_connection:
        start_timestamp, stop_timestamp = db_connection.execute(
            f"""
            SELECT start_timestamp_unix, stop_timestamp_unix from rest_state_task
            WHERE group_session='{group_session}';
        """
        ).fetchall()[0]

        # Update participant ID and label task.
        db_connection.execute(
            f"""
            UPDATE gaze_raw
            SET
                task='rest_state',
                participant='{participant_id}'
            WHERE
                timestamp_unix >= '{start_timestamp}'
                AND timestamp_unix < '{stop_timestamp}'
                and station='{station}'
        """
        )


def label_affective_task_individual_data(
    group_session, station, participant_id, task, db_connection
):
    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")

        # Get start/stop timestamps for affective task
        try:
            start_timestamp = db_connection.execute(
                f"""
                SELECT timestamp_unix from affective_task_event
                WHERE
                    group_session='{group_session}'
                    AND task_type='individual'
                    AND participant='{participant_id}'
                    AND event_type='start_affective_task'
                ORDER BY timestamp_unix LIMIT 1
            """
            ).fetchall()[0][0]
        except IndexError as e:
            error(f"Unable to get starting timestamp for {group_session}, {station}, {participant_id}, {task}")
            raise IndexError(e)

        try:
            stop_timestamp = db_connection.execute(
                f"""
                SELECT timestamp_unix from affective_task_event
                WHERE
                    group_session='{group_session}'
                    AND task_type='individual'
                    AND participant='{participant_id}'
                    AND event_type='final_submission'
                ORDER BY timestamp_unix DESC LIMIT 1
            """
            ).fetchall()[0][0]
        except Exception as e:
            error(f"Unable to get stop timestamp for {group_session}, {station}, {participant_id}, {task}")
            raise Exception(e)

        # Update participant ID and label task.
        db_connection.execute(
            f"""
            UPDATE gaze_raw
            SET
                task='affective_individual',
                participant='{participant_id}'
            WHERE
                timestamp_unix >= '{start_timestamp}'
                AND timestamp_unix <= '{stop_timestamp}'
                AND station='{station}'
        """
        )


def label_affective_task_team_data(
    group_session, station, participant_id, db_connection
):
    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")

        # Get start/stop timestamps for affective task
        start_timestamp = db_connection.execute(
            f"""
            SELECT timestamp_unix from affective_task_event
            WHERE
                group_session='{group_session}'
                AND task_type='team'
                AND event_type='start_affective_task'
            ORDER BY timestamp_unix LIMIT 1
        """
        ).fetchall()[0][0]

        stop_timestamp = db_connection.execute(
            f"""
            SELECT timestamp_unix from affective_task_event
            WHERE
                group_session='{group_session}'
                AND task_type='team'
                AND event_type='final_submission'
            ORDER BY timestamp_unix DESC LIMIT 1
        """
        ).fetchall()[0][0]

        # Update participant ID and label task.
        db_connection.execute(
            f"""
            UPDATE gaze_raw
            SET
                task='affective_team',
                participant='{participant_id}'
            WHERE
                timestamp_unix >= '{start_timestamp}'
                AND timestamp_unix <= '{stop_timestamp}'
                AND station='{station}'
        """
        )


def update_labels(
    group_session, station, participant_id, task, db_connection, table_name
):
    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")

        # Get start/stop timestamps for affective task
        try:
            start_timestamp = db_connection.execute(
                f"""
                SELECT timestamp_unix from {table_name}
                WHERE
                    group_session='{group_session}'
                ORDER BY timestamp_unix LIMIT 1
            """
            ).fetchall()[0][0]
        except IndexError:
            error(f"""IndexError! Cannot update labels for {group_session},
                    {station}, {participant_id}, {task}, {table_name}.""")

        stop_timestamp = db_connection.execute(
            f"""
            SELECT timestamp_unix from {table_name}
            WHERE
                group_session='{group_session}'
            ORDER BY timestamp_unix DESC LIMIT 1
        """
        ).fetchall()[0][0]

        # Update participant ID and label task.
        db_connection.execute(
            f"""
            UPDATE gaze_raw
            SET
                task='{task}',
                participant='{participant_id}'
            WHERE
                timestamp_unix >= '{start_timestamp}'
                AND timestamp_unix <= '{stop_timestamp}'
                AND station='{station}'
        """
        )


def label_minecraft_data(
    group_session, station, participant_id, task, db_connection,
):
    if task == "saturn_a":
        mission = "Saturn_A"
    elif task == "saturn_b":
        mission = "Saturn_B"
    elif task == "hands_on_training":
        mission = "Hands-on Training"
    else:
        raise ValueError(f"Bad task: {task}!")

    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")

        # Get start/stop timestamps for affective task
        start_timestamp = db_connection.execute(
            f"""
            SELECT start_timestamp_unix from mission
            WHERE
                group_session='{group_session}'
                and name = '{mission}'
        """
        ).fetchall()[0][0]

        stop_timestamp = db_connection.execute(
            f"""
            SELECT stop_timestamp_unix from mission
            WHERE
                group_session='{group_session}'
                and name = '{mission}'
        """
        ).fetchall()[0][0]

        # Update participant ID and label task.
        db_connection.execute(
            f"""
            UPDATE gaze_raw
            SET
                task='{task}',
                participant='{participant_id}'
            WHERE
                timestamp_unix >= '{start_timestamp}'
                AND timestamp_unix <= '{stop_timestamp}'
                AND station='{station}'
        """
        )


def label_data():
    info("Labeling data")
    db_connection = sqlite3.connect(DB_PATH)

    with db_connection:
        validity_rows = db_connection.execute(
            f"""
            SELECT * from data_validity
            WHERE modality='gaze';
        """
        ).fetchall()

    for row in tqdm(validity_rows):
        group_session, participant_id, station, task, modality, is_valid = row
        if modality != "gaze":
            continue

        if task == "rest_state":
            if group_session == "exp_2022_12_05_12":
                error(
                    f"""
                    [MISSING DATA] There is no rest state data for
                    {group_session}, due to technical issues. See Rick's email
                    from 2023-07-11 for details."""
                )
            else:
                label_rest_state_task_data(
                    group_session, station, participant_id, db_connection
                )

        elif "affective" in task:
            if task == "affective_individual":
                label_affective_task_individual_data(
                    group_session, station, participant_id, task, db_connection
                )
            elif task == "affective_team":
                label_affective_task_team_data(
                    group_session, station, participant_id, db_connection
                )
            else:
                raise ValueError(f"Bad task: {task}!")

        elif task == "finger_tapping":
            update_labels(
                group_session, station, participant_id, task,
                db_connection, "fingertapping_task_observation"
            )

        elif task == "ping_pong_competitive":
            update_labels(
                group_session, station, participant_id, task,
                db_connection, "ping_pong_competitive_task_observation"
            )

        elif task == "ping_pong_cooperative":
            update_labels(
                group_session, station, participant_id, task,
                db_connection, "ping_pong_cooperative_task_observation"
            )
        else:
            label_minecraft_data(
                group_session, station, participant_id, task, db_connection
            )


def remove_invalid_data():
    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        validity_rows = db_connection.execute(
            f"""
            SELECT * from data_validity
            WHERE modality='gaze';
        """
        ).fetchall()

    gaze_rows = [row for row in validity_rows if row[-2] == "gaze"]

    for row in tqdm(gaze_rows):
        group_session, participant_id, station, task, modality, is_valid = row

        if is_valid == 0:
            info(
                f"Data for task {task} for {group_session} for modality"
                f" {modality} for station {station}/participant {participant_id}"
                " is not valid. We will delete this data from the table."
            )
            db_connection.execute(
                f"""
                DELETE FROM gaze_raw
                WHERE
                    group_session='{group_session}'
                    AND station='{station}'
                    AND task = '{task}'
            """
            )


if __name__ == "__main__":
    info("Starting building Gaze table.")
    recreate_gaze_table()
    create_indices()
    insert_raw_unlabeled_data()
    label_data()
    remove_invalid_data()
    info("Finished building Gaze table.")
