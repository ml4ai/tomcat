#!/usr/bin/env python

import json
import logging
import os
import sys
from glob import glob
from logging import info

import pandas as pd
import pyxdf
from tqdm import tqdm

from datasette_interface.common.config import LOG_DIR, settings
from datasette_interface.common.utils import (
    cd,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
    should_ignore_directory,
)
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.base.data_validity import DataValidity
from datasette_interface.database.entity.task.ping_pong_cooperative_task_observation import (
    PingPongCooperativeTaskObservation,
)

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"{LOG_DIR}/build_ping_pong_cooperative_task_table.log",
            mode="w",
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def process_cooperative_csv_files(csv_file, group_session, participants):
    df = pd.read_csv(
        csv_file,
        delimiter=";",
        dtype=str,
    )

    player_1_id = participants["lion"]
    player_2_id = participants["tiger"]
    player_3_id = participants["leopard"]

    ping_pong_observations = []
    for i, row in df.iterrows():
        current_started_value = row["started"]

        seconds = int(row["seconds"])
        # For some reason, pygame sometimes outputs a negative value for seconds
        # elapsed - in this case, the baseline task program writes a
        # value of '110' for the countdown timer. We have seen this occur
        # so far whenever the value in the `started` column changes, for
        # the first value after this change occurs.
        # will replace such values with 120 (the initial value).
        if i != 0:
            previous_started_value = df.loc[i - 1]["started"]
            if (current_started_value != previous_started_value) and (seconds == 110):
                seconds = 120

        if group_session == "exp_2023_01_31_14":
            # There is no AI player's paddle position in the .csv file for this experiment. Set it
            # to NULL.
            (
                ball_x,
                ball_y,
                player_1_x,
                player_1_y,
                player_2_x,
                player_2_y,
                player_3_x,
                player_3_y,
            ) = row.iloc[-9:-1]
            ai_x = None
            ai_y = None
        else:
            (
                ball_x,
                ball_y,
                player_1_x,
                player_1_y,
                player_2_x,
                player_2_y,
                player_3_x,
                player_3_y,
                ai_x,
                ai_y,
            ) = row.iloc[-11:-1]

        ping_pong_observation = PingPongCooperativeTaskObservation(
            group_session_id=group_session,
            player_1_id=player_1_id,
            player_2_id=player_2_id,
            player_3_id=player_3_id,
            timestamp_unix=row["time"],
            timestamp_iso8601=convert_unix_timestamp_to_iso8601(df["time"].iloc[i]),
            task_started=bool(current_started_value),
            seconds=int(seconds),
            ball_position_x=int(ball_x),
            ball_position_y=int(ball_y),
            player_1_paddle_position_x=int(player_1_x),
            player_1_paddle_position_y=int(player_1_y),
            player_2_paddle_position_x=int(player_2_x),
            player_2_paddle_position_y=int(player_2_y),
            player_3_paddle_position_x=int(player_3_x),
            player_3_paddle_position_y=int(player_3_y),
            ai_paddle_position_x=int(ai_x) if ai_x else None,
            ai_paddle_position_y=int(ai_y) if ai_y else None,
            team_score=int(row["score_left"]),
            ai_score=int(row["score_right"]),
        )
        ping_pong_observations.append(ping_pong_observation)

    return ping_pong_observations


def process_directory_v1(group_session, participants):
    with cd(f"{group_session}/baseline_tasks/ping_pong"):
        cooperative_csv_files = glob("cooperative*.csv")
        assert len(cooperative_csv_files) == 1

        if group_session == "exp_2023_01_31_14":
            info(
                "[CORRECTION]: For the cooperative ping-pong task in the exp_2023_01_31_14"
                " session, the CSV file has no columns indicating the AI player's paddle position."
                " We set the AI player's paddle positions to NULL in the database for this "
                "experiment."
            )

        return process_cooperative_csv_files(
            cooperative_csv_files[0], group_session, participants
        )


def process_directory_v2(group_session, participants):
    """Process directory assuming unified XDF files."""
    with cd(f"{group_session}/lsl"):
        streams, header = pyxdf.load_xdf(
            "block_1.xdf", select_streams=[{"name": "PingPong_cooperative_0"}]
        )
        assert len(streams) == 1
        stream = streams[0]

        ping_pong_observations = []
        for i, (timestamp, data) in enumerate(
            zip(stream["time_stamps"], stream["time_series"])
        ):
            data = json.loads(data[0])
            current_started_value = data["started"]

            seconds = int(data["seconds"])
            # For some reason, pygame sometimes outputs a negative value for seconds
            # elapsed - in this case, the baseline task program writes a
            # value of '110' for the countdown timer. We have seen this occur
            # so far whenever the value in the `started` column changes, for
            # the first value after this change occurs.
            # will replace such values with 120 (the initial value).
            if i != 0:
                previous_started_value = json.loads(stream["time_series"][i - 1][0])[
                    "started"
                ]
                if (current_started_value != previous_started_value) and (
                    seconds == 110
                ):
                    seconds = 120

            (
                ball_x,
                ball_y,
                player_1_x,
                player_1_y,
                player_2_x,
                player_2_y,
                player_3_x,
                player_3_y,
                ai_x,
                ai_y,
            ) = list(data.values())[-11:-1]

            ping_pong_observation = PingPongCooperativeTaskObservation(
                group_session_id=group_session,
                player_1_id=participants["lion"],
                player_2_id=participants["tiger"],
                player_3_id=participants["leopard"],
                timestamp_unix=data["time"],
                timestamp_iso8601=convert_unix_timestamp_to_iso8601(data["time"]),
                task_started=bool(current_started_value),
                seconds=int(seconds),
                ball_position_x=int(ball_x),
                ball_position_y=int(ball_y),
                player_1_paddle_position_x=int(player_1_x),
                player_1_paddle_position_y=int(player_1_y),
                player_2_paddle_position_x=int(player_2_x),
                player_2_paddle_position_y=int(player_2_y),
                player_3_paddle_position_x=int(player_3_x),
                player_3_paddle_position_y=int(player_3_y),
                ai_paddle_position_x=int(ai_x),
                ai_paddle_position_y=int(ai_y),
                team_score=int(data["score_left"]),
                ai_score=int(data["score_right"]),
            )
            ping_pong_observations.append(ping_pong_observation)

    return ping_pong_observations


def process_ping_pong_cooperative_task_data():
    info(
        """
        Processing ping pong cooperative task data. For the CSV files predating the
        unified XDF file era, We will use the `time` column in the CSV,"
        ignoring the `monotonic_time` and `human_readable` time columns,
        since those timestamps are systematically a few microseconds later
        than the timestamps in the `time` column, since they are created by
        separate invocations to monotonic() and datetime.utcnow() respectively.
        """
    )

    info("Processing directories...")

    with cd(settings.experiment_root_dir):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        db_session = next(get_db())
        processed_group_sessions = set(
            (
                [
                    s[0]
                    for s in db_session.query(
                        PingPongCooperativeTaskObservation.group_session_id
                    )
                    .distinct(PingPongCooperativeTaskObservation.group_session_id)
                    .all()
                ]
            )
        )

        for group_session in tqdm(sorted(directories_to_process), unit="directories"):
            if group_session in processed_group_sessions:
                info(
                    f"Found saved ping-pong cooperative data for {group_session} in the database. "
                    f"Skipping group session."
                )
                continue

            info(f"Processing directory {group_session}")
            # Get real participant IDs for the task
            participants = {}
            for station in ["lion", "tiger", "leopard"]:
                participant = (
                    db_session.query(DataValidity.participant_id)
                    .filter_by(
                        group_session_id=group_session,
                        task_id="ping_pong_cooperative",
                        station_id=station,
                    )
                    .first()[0]
                )
                participants[station] = participant
            if not is_directory_with_unified_xdf_files(group_session):
                ping_pong_observations = process_directory_v1(
                    group_session, participants
                )
            else:
                ping_pong_observations = process_directory_v2(
                    group_session, participants
                )

            if len(ping_pong_observations) > 0:
                db_session.add_all(ping_pong_observations)
                db_session.commit()
        db_session.close()
