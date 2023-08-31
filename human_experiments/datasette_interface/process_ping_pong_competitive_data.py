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
from entity.task.ping_pong_competitive_task_observation import PingPongCompetitiveTaskObservation
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
            filename=f"/space/{USER}/tomcat/build_ping_pong_competitive_task_table.log",
            mode="w",
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def process_competitive_csv_files(competitive_csv_files, group_session, participants):
    ping_pong_observations = []
    for csv_file in competitive_csv_files:
        df = pd.read_csv(
            csv_file,
            delimiter=";",
            dtype=str,
        )

        left_station, right_station = (
            ["lion", "tiger"] if "_0" in csv_file else ["leopard", "cheetah"]
        )

        player_1_id = participants[left_station]
        # Player 2 is an experimenter on cheetah, but we don't know
        # which experimenter.
        player_2_id = (
            participants[right_station] if right_station != "cheetah" else -3
        )

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
            ) = row.iloc[-7:-1]

            ping_pong_observation = PingPongCompetitiveTaskObservation(
                group_session_id=group_session,
                player_1_id=player_1_id,
                player_2_id=player_2_id,
                player_1_station_id=left_station,
                player_2_station_id=right_station,
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
                player_1_score=int(row["score_left"]),
                player_2_score=int(row["score_right"])
            )
            ping_pong_observations.append(ping_pong_observation)

    return ping_pong_observations


def process_directory_v1(group_session, participants):
    with cd(f"{group_session}/baseline_tasks/ping_pong"):
        competitive_csv_files = glob("competitive*.csv")
        assert len(competitive_csv_files) == 2
        return process_competitive_csv_files(competitive_csv_files, group_session, participants)


def process_directory_v2(group_session, participants):
    """Process directory assuming unified XDF files."""
    debug(f"Processing directory {group_session}")

    ping_pong_observations = []
    with cd(f"{group_session}/lsl"):
        streams, header = pyxdf.load_xdf(
            "block_1.xdf", select_streams=[{"type": "ping_pong"}]
        )
        streams = [
            stream
            for stream in streams
            if stream["info"]["name"][0] != "PingPong_cooperative_0"
        ]

        assert len(streams) == 2

        for stream in streams:
            stream_name = stream["info"]["name"][0]

            left_station, right_station = (
                ["lion", "tiger"]
                if "_0" in stream_name
                else ["leopard", "cheetah"]
            )

            player_1_id = participants[left_station]
            # Player 2 is an experimenter on cheetah, but we don't know
            # which experimenter.
            player_2_id = (
                participants[right_station]
                if right_station != "cheetah"
                else -3
            )

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
                    previous_started_value = json.loads(
                        stream["time_series"][i - 1][0]
                    )["started"]
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
                ) = list(data.values())[-7:-1]

                ping_pong_observation = PingPongCompetitiveTaskObservation(
                    group_session_id=group_session,
                    player_1_id=player_1_id,
                    player_2_id=player_2_id,
                    player_1_station_id=left_station,
                    player_2_station_id=right_station,
                    timestamp_unix=data["time"],
                    timestamp_iso8601=convert_unix_timestamp_to_iso8601(data["time"]),
                    task_started=current_started_value,
                    seconds=seconds,
                    ball_position_x=ball_x,
                    ball_position_y=ball_y,
                    player_1_paddle_position_x=player_1_x,
                    player_1_paddle_position_y=player_1_y,
                    player_2_paddle_position_x=player_2_x,
                    player_2_paddle_position_y=player_2_y,
                    player_1_score=data["score_left"],
                    player_2_score=data["score_right"]
                )
                ping_pong_observations.append(ping_pong_observation)

    return ping_pong_observations


def process_ping_pong_competitive_task_data(database_engine):
    info(
        """
        Processing ping pong competitive task data. For the CSV files predating the
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
            for group_session in tqdm(sorted(directories_to_process), unit="directories"):
                # Get real participant IDs for the task
                participants = {}
                for station in ["lion", "tiger", "leopard"]:
                    participant = database_session.query(DataValidity.participant_id).filter_by(
                        group_session_id=group_session,
                        task_id="ping_pong_competitive",
                        station_id=station).first()[0]
                    participants[station] = participant

                if not is_directory_with_unified_xdf_files(group_session):
                    ping_pong_observations.extend(process_directory_v1(group_session, participants))
                else:
                    ping_pong_observations.extend(process_directory_v2(group_session, participants))

            info("Adding ping-pong competitive observations to the database.")
            database_session.add_all(ping_pong_observations)
            database_session.commit()


def recreate_ping_pong_competitive_observation_table(database_engine):
    PingPongCompetitiveTaskObservation.__table__.drop(database_engine, checkfirst=True)
    PingPongCompetitiveTaskObservation.__table__.create(database_engine, checkfirst=True)
