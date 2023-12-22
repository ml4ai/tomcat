#!/usr/bin/env python

"""
This script is responsible for estimating and persisting to the ToMCAT database vocalic features
extracted from audio files in the experiments directories. We use OpenSmile to do so, with
configuration files defined under data_pre_processing/audio/opensmile.
"""
import logging
import os.path
import sys

import pandas as pd

from datasette_interface.model.audio.pcm_audio import PCMAudio
from datasette_interface.common.experiments_crawler import ExperimentsCrawler
from datasette_interface.common.utils import \
    convert_unix_timestamp_to_iso8601, convert_iso8601_timestamp_to_unix
from datasette_interface.database.entity.task.minecraft_task import MinecraftMission
from datasette_interface.database.entity.derived.audio_vocalics import AudioVocalics
from datasette_interface.database.config import get_db
from logging import info
from datasette_interface.common.config import settings, LOG_DIR
from sqlalchemy import select
from datetime import datetime


def extract_vocalic_features_callback(experiment_dir: str, has_unified_xdf: bool):
    """
    Callback function passed to the experiments crawler to process an experiment in order to
    extract vocalic features from the audio files in it.

    :param experiment_dir: directory where the experiment data is located.
    :param has_unified_xdf: whether the experiment has unified xdf files.
    """
    info(f"Extracting vocalic features from {experiment_dir}.")

    db_session = next(get_db())
    for station in ["lion", "tiger", "leopard"]:
        info(f"Extracting vocalics from {station}.")

        audio_dir = (
            f"{experiment_dir}/{station}/audio/block_2"
            if has_unified_xdf
            else f"{experiment_dir}/{station}/audio"
        )
        if not os.path.exists(audio_dir):
            info("No audio directory found.")
            continue

        audio_files = sorted(os.listdir(audio_dir))
        if len(audio_files) == 0:
            info("No audio file found.")
            continue

        for audio_filename in audio_files:
            audio_filepath = f"{audio_dir}/{audio_filename}"
            if not os.path.isfile(audio_filepath) or audio_filename[-4:] != ".wav":
                continue

            minecraft_mission_id = audio_filename[
                                   audio_filename.find("-") + 1: audio_filename.find("_Team")
                                   ]
            group_session = experiment_dir[experiment_dir.rfind("/") + 1:]

            if db_session.scalar(select(AudioVocalics.timestamp_unix).where(
                AudioVocalics.group_session_id == group_session,
                AudioVocalics.station_id == station,
                AudioVocalics.minecraft_mission_id == minecraft_mission_id)) is not None:
                info(
                    f"Found saved audio vocalics for {group_session}, {station}, and "
                    f"{minecraft_mission_id} in the database. Skipping parsing.")
                continue

            info(f"Extracting vocalics from {audio_filename}.")

            first_timestamp_unix = db_session.scalar(
                select(MinecraftMission.trial_start_timestamp_unix).where(
                    MinecraftMission.group_session_id == group_session,
                    MinecraftMission.id == minecraft_mission_id
                ))

            if first_timestamp_unix is None:
                info(
                    f"Skipping {audio_filename} because it's not associated to a valid "
                    f"Minecraft mission."
                )
                continue

            # Some audios may be broken because the part of the header that defines the file size
            # was not filled. This happened in early experiments but it was solved later. So, here
            # we fix this issue by saving a copy of the audio with the header fixed in a temporary
            # folder and we extract vocalic from this fixed version of the audio.
            audio = PCMAudio(audio_filepath)

            fixed_audio_filepath = f"{settings.tmp_dir}/{audio_filename}"
            audio.fix_header(fixed_audio_filepath)
            audio = PCMAudio(fixed_audio_filepath)

            # This will extract vocalic features using the OpenSmile CLI command not the python
            # library. This is to conform with the format get when executing from the CLI, such as
            # column names and frame count colum, which could not be directly reproduced in the
            # python version. It also allows anyone to use our opensmile config files to extract
            # vocalics from their audios without having to write a python script for it.
            audio_filename_no_extension = audio_filename[: audio_filename.rfind(".")]
            vocalics_filepath = f"{settings.tmp_dir}/{audio_filename_no_extension}.csv"
            if not audio.extract_vocalic_features(vocalics_filepath):
                continue

            # Parses the saved .csv file with the vocalics to assemble persistent objects to be s
            # saved to the database.
            df = pd.read_csv(vocalics_filepath, sep=";")
            df = df.drop("name", axis=1)
            df = df.rename(columns={"frameTime": "frame_time"})

            vocalics_columns = list(df.columns)

            df["group_session_id"] = group_session
            df["station_id"] = station
            df["minecraft_mission_id"] = minecraft_mission_id
            df["timestamp_unix"] = df["frame_time"] + first_timestamp_unix
            df["timestamp_iso8601"] = df["timestamp_unix"].apply(
                convert_unix_timestamp_to_iso8601
            )

            # To conform with the other tables, save this as a string.
            df["timestamp_unix"] = df["timestamp_unix"].astype(str)

            # Place group_session, station, minecraft_mission_id, id, timestamp_unix,
            # timestamp_iso8601 in the beginning and transform vocalic features to lower case.
            df = df.reset_index().rename(columns={"index": "id"})
            df = df[["group_session_id", "station_id", "minecraft_mission_id", "id",
                     "timestamp_unix", "timestamp_iso8601"] + vocalics_columns]

            def format_column_name(column_name: str) -> str:
                return column_name.lower().replace(
                    "-", "_").replace(
                    ".", "_").replace(
                    "[", "").replace(
                    "]", "")

            df.columns = map(format_column_name, df.columns)

            info("Converting DataFrame to records.")
            records = df.to_dict("records")
            vocalics_data = []
            for record in records:
                vocalics_data.append(AudioVocalics(**record))

            info("Saving to the database.")
            db_session.add_all(vocalics_data)
            db_session.commit()

            info("Removing temporary files")
            os.remove(fixed_audio_filepath)
            os.remove(vocalics_filepath)
    db_session.close()


logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"{LOG_DIR}/extract_vocalic_features.log",
            mode="w",
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)

crawler = ExperimentsCrawler(callback=extract_vocalic_features_callback)
crawler.crawl()
