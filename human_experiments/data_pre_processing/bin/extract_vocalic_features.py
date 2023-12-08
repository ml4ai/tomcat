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
from sqlalchemy import create_engine, text

from data_pre_processing.audio.pcm_audio import PCMAudio
from data_pre_processing.common.constants import DEBUG, TMP_DIR
from data_pre_processing.common.experiments_crawler import ExperimentsCrawler
from data_pre_processing.common.formatting import \
    convert_unix_timestamp_to_iso8601
from data_pre_processing.database.config import TOMCAT_DATABASE_ENGINE

info = logging.getLogger().info
error = logging.getLogger().error

# Sve to a local slite DB if the script is run in debug mode.
if DEBUG:
    TARGET_DATABASE_ENGINE = create_engine(f"sqlite:////{TMP_DIR}/tomcat_tmp.db")
else:
    TARGET_DATABASE_ENGINE = TOMCAT_DATABASE_ENGINE


def extract_vocalic_features_callback(experiment_dir: str, has_unified_xdf: bool):
    """
    Callback function passed to the experiments crawler to process an experiment in order to
    extract vocalic features from the audio files in it.

    :param experiment_dir: directory where the experiment data is located.
    :param has_unified_xdf: whether the experiment has unified xdf files.
    """
    info(f"Extracting vocalic features from {experiment_dir}")

    for station in ["lion", "tiger", "leopard"]:
        info(f"Extracting vocalics from {station}")

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
                audio_filename.find("-") + 1 : audio_filename.find("_Team")
            ]
            group_session = experiment_dir[experiment_dir.rfind("/") + 1 :]

            minecraft_query = f"""
                SELECT
                    trial_start_timestamp_unix
                FROM
                    minecraft_mission
                WHERE group_session = '{group_session}' 
                  AND id = '{minecraft_mission_id}'
            """
            minecraft_df = pd.DataFrame(
                TOMCAT_DATABASE_ENGINE.connect().execute(text(minecraft_query))
            )

            if len(minecraft_df) == 0:
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

            fixed_audio_filepath = f"{TMP_DIR}/{audio_filename}"
            audio.fix_header(fixed_audio_filepath)
            audio = PCMAudio(fixed_audio_filepath)

            # This will extract vocalic features using the OpenSmile CLI command not the python
            # library. This is to conform with the format get when executing from the CLI, such as
            # column names and frame count colum, which could not be directly reproduced in the
            # python version. It also allows anyone to use our opensmile config files to extract
            # vocalics from their audios without having to write a python script for it.
            audio_filename_no_extension = audio_filename[: audio_filename.rfind(".")]
            vocalics_filepath = f"{TMP_DIR}/{audio_filename_no_extension}.csv"
            if not os.path.exists(vocalics_filepath):
                # Avoid generating twice if the process broke before but vocalics were generated.
                if not audio.extract_vocalic_features(vocalics_filepath):
                    continue

            # Parses the saved .csv file with the vocalics to assemble persistent objects to be s
            # saved to the database.
            df = pd.read_csv(vocalics_filepath, sep=";")
            df = df.drop("name", axis=1)

            df["group_session"] = group_session
            df["station"] = station
            df["minecraft_mission_id"] = minecraft_mission_id
            df = df[
                ["group_session", "station", "minecraft_mission_id"]
                + list(df.columns[:-3])
            ]

            first_timestamp_unix = float(
                minecraft_df.iloc[0]["trial_start_timestamp_unix"]
            )
            df["timestamp_unix"] = df["frameTime"] + first_timestamp_unix
            df = df.rename(columns={"frameTime": "frame_time"})
            df["timestamp_iso8601"] = df["timestamp_unix"].apply(
                convert_unix_timestamp_to_iso8601
            )

            # Place group_session, minecraft_mission_id, frame_time, timestamp_unix and
            # timestamp_iso8601 in the beginning and transform vocalic features to lower case.
            df = df[
                list(df.columns[:4])
                + ["timestamp_unix", "timestamp_iso8601"]
                + list(df.columns[4:-2])
            ]
            df.columns = [c.lower() for c in df.columns]

            # To conform with the other tables, save this as a string.
            df["timestamp_unix"] = df["timestamp_unix"].astype(str)

            try:
                df.to_sql(
                    "audio_vocalics",
                    TARGET_DATABASE_ENGINE,
                    index=False,
                    if_exists="append",
                    method="multi",
                )
                os.remove(fixed_audio_filepath)
                os.remove(vocalics_filepath)
            except Exception as ex:
                error(f"Could not save vocalics to the database. {ex}")

            try:
                if not DEBUG:
                    # We need to set the primary key manually because pandas to_sql has no option
                    # to do so.
                    with TARGET_DATABASE_ENGINE.connect() as con:
                        con.execute(
                            text(
                                "ALTER TABLE audio_vocalics ADD PRIMARY KEY "
                                "(group_session, station, minecraft_mission_id, "
                                "frame_time);"
                            )
                        )
                        con.commit()
            except Exception:
                # Do nothing. It will complain when we try to set the primary key again anyway.
                pass


logging.basicConfig(level=logging.INFO)
crawler = ExperimentsCrawler(callback=extract_vocalic_features_callback)
crawler.crawl()
