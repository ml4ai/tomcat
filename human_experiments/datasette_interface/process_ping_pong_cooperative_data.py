#!/usr/bin/env python

import json
import logging
import os
import sys
from glob import glob
from logging import info, debug

import pandas as pd
import pyxdf
from sqlalchemy.orm import Session
from tqdm import tqdm

from config import USER
from entity.base.data_validity import DataValidity
from entity.task.ping_pong_cooperative_task_observation import PingPongCooperativeTaskObservation
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
            filename=f"/space/{USER}/tomcat/build_ping_pong_cooperative_task_table.log",
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
            if (
                current_started_value != previous_started_value
            ) and (seconds == 110):
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
            ai_paddle_position_x=int(ai_x),
            ai_paddle_position_y=int(ai_y),
            team_score=int(row["score_left"]),
            ai_score=int(row["score_right"])
        )
        ping_pong_observations.append(ping_pong_observation)

    return ping_pong_observations


def process_directory_v1(group_session, participants):
    info(f"Processing directory {group_session}")
    with cd(f"{group_session}/baseline_tasks/ping_pong"):
        cooperative_csv_files = glob("cooperative*.csv")
        assert len(cooperative_csv_files) == 1
        return process_cooperative_csv_files(cooperative_csv_files[0], group_session, participants)


def process_directory_v2(group_session, participants):
    """Process directory assuming unified XDF files."""
    debug(f"Processing directory {group_session}")

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
                previous_started_value = json.loads(stream["time_series"][i - 1][0])["started"]
                if (
                    current_started_value != previous_started_value
                ) and (seconds == 110):
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
                ai_score=int(data["score_right"])
            )
            ping_pong_observations.append(ping_pong_observation)

    return ping_pong_observations


def process_ping_pong_cooperative_task_data(database_engine):
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

    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        ping_pong_observations = []
        with Session(database_engine) as database_session:
            for group_session in tqdm(
                sorted(directories_to_process), unit="directories"
            ):
                # Get real participant IDs for the task
                participants = {}
                for station in ["lion", "tiger", "leopard"]:
                    participant = database_session.query(DataValidity.participant_id).filter_by(
                        group_session_id=group_session,
                        task_id="ping_pong_cooperative",
                        station_id=station).first()[0]
                    participants[station] = participant
                if not is_directory_with_unified_xdf_files(group_session):
                    ping_pong_observations.extend(process_directory_v1(group_session, participants))
                else:
                    ping_pong_observations.extend(process_directory_v2(group_session, participants))

            info("Adding ping-pong cooperative observations to the database.")
            database_session.add_all(ping_pong_observations)
            database_session.commit()


def recreate_ping_pong_cooperative_observation_table(database_engine):
    PingPongCooperativeTaskObservation.__table__.drop(database_engine, checkfirst=True)
    PingPongCooperativeTaskObservation.__table__.create(database_engine, checkfirst=True)
