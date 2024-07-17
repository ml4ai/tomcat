#!/usr/bin/env python
"""Script to process testbed messages"""
import json
import logging
import os
import sys
from glob import glob
from logging import debug, error, info
from pprint import pprint

import pyxdf
from tqdm import tqdm

from datasette_interface.common.config import LOG_DIR, settings
from datasette_interface.common.utils import (
    cd,
    convert_iso8601_timestamp_to_unix,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
    should_ignore_directory,
)
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.task.minecraft_task import (
    MinecraftMission,
    MinecraftTestbedMessage,
)
from datasette_interface.database.entity.base.data_validity import DataValidity
from datasette_interface.database.entity.base.group_session import GroupSession
from sqlalchemy import select
from sqlalchemy.exc import NoResultFound, MultipleResultsFound


logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"{LOG_DIR}/build_testbed_message_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)

# Duplicate, invalid missions. Do not save to the database
INVALID_MISSIONS = [
    "560d4c45-dc45-4e19-bdb3-e4e15021728a",  # exp_2022_10_07_15 Saturn A with timestamp in 03/2022
    "a48f475f-40b0-46b9-8284-0db267dddb67",  # exp_2022_10_07_15 Saturn A with small duration
    # exp_2023_02_07_14 Hands-On Training with small duration
    "171a8713-a554-4d8e-a4b1-3ec1b728d0a2",
    # exp_2023_02_10_10 Hands-On Training with small duration
    "9cde1985-1179-4aac-8b67-1fc60ed65243",
]

CALLSIGN_TO_STATION_MAPPING = {
    "Red": "lion",
    "Blue": "tiger",
    "Green": "leopard",
}


def populate_station_to_playername_mapping(
    client_info: dict, db_session, group_session
):
    assert len(client_info) == 3
    for client in client_info:
        try:
            gs = db_session.execute(
                select(GroupSession).filter_by(id=group_session)
            ).scalar_one()
            station = CALLSIGN_TO_STATION_MAPPING[client["callsign"]]
            setattr(
                gs, f"{station}_minecraft_playername", client["playername"]
            )
        except NoResultFound as e:
            error(
                f"No row in the group_session table found with {group_session=}"
            )


def update_key_messages_dict(message, key_messages):
    """Populate key messages dictionary."""
    topic = message["topic"]
    if topic == "trial":
        sub_type = message["msg"]["sub_type"]
        if sub_type == "start":
            info(
                "Trial start message detected"
                f" ({message['data']['experiment_mission']}) with"
                f" .header.timestamp {message['header']['timestamp']}"
            )
            key_messages["trial_start"].append(message)
        elif sub_type == "stop":
            info(
                "Trial stop message detected"
                f" ({message['data']['experiment_mission']}) with"
                f" .header.timestamp {message['header']['timestamp']}"
            )
            key_messages["trial_stop"].append(message)
        else:
            pass
    elif topic == "observations/events/mission":
        mission_state = message["data"]["mission_state"]
        if mission_state == "Start":
            info(
                "Mission start message detected"
                f" ({message['data']['mission']}) with"
                f" .header.timestamp {message['header']['timestamp']}"
            )
            key_messages["mission_start"].append(message)
        elif mission_state == "Stop":
            info(
                "Mission stop message detected"
                f" ({message['data']['mission']}) with"
                f" .header.timestamp {message['header']['timestamp']}"
            )
            key_messages["mission_stop"].append(message)
        else:
            pass
    elif (
        topic == "observations/state"
        and not key_messages["approximate_mission_start"]
    ):
        mission_timer = str(message["data"]["mission_timer"])
        if "not initialized" not in mission_timer.lower():
            key_messages["approximate_mission_start"] = message


def get_key_messages(metadata_file):
    """Get key messages from a .metadata file."""
    key_messages = {
        "trial_start": [],
        "trial_stop": [],
        "mission_start": [],
        "mission_stop": [],
        "approximate_mission_start": None,
    }

    with open(metadata_file) as f:
        info(f"Inspecting {metadata_file} to get key messages.")
        for line in f:
            message = json.loads(line)
            update_key_messages_dict(message, key_messages)

    return key_messages


def collect_files_to_process(metadata_files):
    missions = {}
    file_to_key_messages_mapping = {}

    for metadata_file in metadata_files:
        key_messages = get_key_messages(metadata_file)

        if len(key_messages["mission_start"]) > 1:
            error(
                "[ANOMALY]: We expected exactly 1 mission start message, but"
                f" found {len(key_messages['mission_start'])} instead!"
                " Skipping this file since we do not know the cause of this error."
            )
            continue

        if len(key_messages["trial_start"]) == 0:
            error(
                "[ANOMALY]: We expected at least 1 trial start message but found 0."
                " Skipping this file since we do not know the cause of this error."
            )
            continue

        if len(key_messages["trial_stop"]) == 0:
            error(
                "[ANOMALY]: We expected at least 1 trial stop message, but found 0."
                " Skipping this file since we do not know the cause of this error."
            )
            continue

        if len(key_messages["mission_start"]) == 0:
            if key_messages["approximate_mission_start"]:
                error(
                    "[ANOMALY]: We expected exactly 1 mission start message, but found 0 instead! "
                    "Using the timestamp of the first observations/state message as the mission "
                    "start."
                )
                approximate_timestamp = key_messages[
                    "approximate_mission_start"
                ]["header"]["timestamp"]

                # Create a fake mission start message with the approximate timestamp
                key_messages["mission_start"].append(
                    {"header": {"timestamp": approximate_timestamp}}
                )
            else:
                error(
                    "[ANOMALY]: We expected exactly 1 mission start message, but"
                    " found 0 instead and no observation messages detected!"
                    " Skipping this file since we do not know the cause of this error."
                )
                continue

        if len(key_messages["trial_start"]) > 1:
            error(
                "[ANOMALY]: Ideally there would only be 1 trial start message,"
                f" but we found {len(key_messages['trial_start'])}."
                " We think this is due to a an older version of Paulo's Elasticsearch export"
                " script being run. But we do *not* skip this file, we will simply"
                " use the most recent trial start message"
            )
            key_messages["trial_start"] = [
                sorted(
                    key_messages["trial_start"],
                    key=lambda message: message["header"]["timestamp"],
                )[-1]
            ]

        if len(key_messages["trial_stop"]) > 1:
            error(
                "[ANOMALY]: Ideally there would only be 1 trial stop message,"
                f" but we found {len(key_messages['trial_stop'])}."
                " We think this is due to a an older version of Paulo's Elasticsearch export"
                " script being run. But we do *not* skip this file, we will simply"
                " use the most recent trial stop message"
            )
            key_messages["trial_stop"] = [
                sorted(
                    key_messages["trial_stop"],
                    key=lambda message: message["header"]["timestamp"],
                )[-1]
            ]

        if len(key_messages["mission_stop"]) != 1:
            error(
                f"[ANOMALY]: {len(key_messages['mission_stop'])} mission stop messages found!"
                " This could be because of a participant getting sick, or some other reason."
                " Thus, we do *not* skip this file."
            )

        file_to_key_messages_mapping.update({metadata_file: key_messages})
        mission = key_messages["mission_start"][0]["data"]["mission"]

        if mission not in missions:
            missions[mission] = {metadata_file: key_messages}
        else:
            missions[mission][metadata_file] = key_messages

    for mission, files in missions.items():
        files_to_ignore = []
        if len(files) > 1:
            info(
                f"More than one .metadata file matches the {mission} mission."
            )
            for file, key_messages in sorted(files.items()):
                info(
                    f"{key_messages['mission_start'][0]['header']['timestamp']}\t{file}"
                )

            files_sorted_by_timestamp_in_descending_order = [
                file
                for (file, key_messages) in sorted(
                    files.items(),
                    key=lambda x: x[1]["mission_start"][0]["header"][
                        "timestamp"
                    ],
                    reverse=True,
                )
            ]

            most_recent_file = files_sorted_by_timestamp_in_descending_order[0]
            files_to_ignore.extend(
                files_sorted_by_timestamp_in_descending_order[1:]
            )

            info(
                "We will select the most recent one by inspecting the .header.timestamp"
                " value of the first trial start message in the .metadata file."
                f"\nMost recent .metadata file: {most_recent_file}"
            )
            info(f"Ignoring files: {files_to_ignore}")
            for file in files_to_ignore:
                del files[file]

    all_missions = {"Hands-on Training", "Saturn_A", "Saturn_B"}
    missing_missions = all_missions - set(missions.keys())
    if len(missing_missions) != 0:
        info(f"[MISSING DATA]: Missing missions: {missing_missions}")

    return file_to_key_messages_mapping


def process_directory_v1(group_session, db_session):
    """Process directory assuming it is from before we had the unified XDF files."""

    minecraft_missions = []
    testbed_messages = []
    try:
        with cd(f"{group_session}/minecraft"):
            metadata_files = sorted(glob("*.metadata"))
            file_to_key_messages_mapping = collect_files_to_process(
                metadata_files
            )
            for metadata_file in file_to_key_messages_mapping:
                info(f"\tProcessing {metadata_file}")
                mission, messages = process_metadata_file(
                    metadata_file,
                    group_session,
                    file_to_key_messages_mapping,
                    db_session,
                )
                if mission and messages:
                    minecraft_missions.append(mission)
                    testbed_messages.extend(messages)
    except FileNotFoundError:
        error(
            f"[MISSING DATA]: No 'minecraft' directory found in {group_session}. Skipping the "
            f"directory!"
        )

    return minecraft_missions, testbed_messages


def process_metadata_file(
    filepath, group_session, file_to_key_messages_mapping, db_session
):
    scores = []
    key_messages = file_to_key_messages_mapping[filepath]

    trial_id = key_messages["mission_start"][0]["msg"]["trial_id"]

    if trial_id in INVALID_MISSIONS:
        error(
            f"[ANOMALY] Skipping {trial_id} as it is a duplicate not removed by the deduplicate "
            f"logic."
        )
        return None, None

    messages_to_insert_into_db = []
    with open(filepath) as f:
        mission_in_progress = False
        for line in f:
            message = json.loads(line)
            topic = message["topic"]

            if topic == "observations/events/mission":
                if message["data"]["mission_state"] == "Start":
                    mission_in_progress = True

            if topic == "observations/events/mission":
                if message["data"]["mission_state"] == "Stop":
                    mission_in_progress = False

            if topic == "observations/events/scoreboard":
                score = message["data"]["scoreboard"]["TeamScore"]
                scores.append(score)

            if mission_in_progress:
                messages_to_insert_into_db.append(message)

    mission_start_timestamp = key_messages["mission_start"][0]["header"][
        "timestamp"
    ]

    mission_stop_timestamp = (
        key_messages["mission_stop"][0]["header"]["timestamp"]
        if len(key_messages["mission_stop"]) == 1
        else key_messages["trial_stop"][0]["header"]["timestamp"]
    )

    # Important to align audio with data
    trial_start_timestamp = key_messages["trial_start"][0]["header"][
        "timestamp"
    ]
    trial_stop_timestamp = key_messages["trial_stop"][0]["header"]["timestamp"]

    mission_name = key_messages["mission_start"][0]["data"]["mission"]

    testbed_version = key_messages["trial_start"][-1]["data"][
        "testbed_version"
    ]

    if len(scores) == 0:
        error(
            f"[MISSING DATA]: No scoreboard messages found in {filepath}!"
            " This could be a bug in the testbed."
        )
        final_team_score = None
    else:
        final_team_score = scores[-1]

    minecraft_mission = MinecraftMission(
        group_session_id=group_session,
        id=trial_id,
        name=mission_name,
        mission_start_timestamp_unix=convert_iso8601_timestamp_to_unix(
            mission_start_timestamp
        ),
        mission_start_timestamp_iso8601=mission_start_timestamp,
        mission_stop_timestamp_unix=convert_iso8601_timestamp_to_unix(
            mission_stop_timestamp
        ),
        mission_stop_timestamp_iso8601=mission_stop_timestamp,
        trial_start_timestamp_unix=convert_iso8601_timestamp_to_unix(
            trial_start_timestamp
        ),
        trial_start_timestamp_iso8601=trial_start_timestamp,
        trial_stop_timestamp_unix=convert_iso8601_timestamp_to_unix(
            trial_stop_timestamp
        ),
        trial_stop_timestamp_iso8601=trial_stop_timestamp,
        final_team_score=final_team_score,
        testbed_version=testbed_version,
    )

    minecraft_testbed_messages = [
        MinecraftTestbedMessage(
            mission_id=trial_id,
            id=i,
            timestamp_unix=convert_iso8601_timestamp_to_unix(
                message["header"]["timestamp"]
            ),
            timestamp_iso8601=message["header"]["timestamp"],
            topic=message.pop("topic"),
            message=json.dumps(message),
        )
        for i, message in enumerate(messages_to_insert_into_db)
    ]

    # Here, we try to map Minecraft 'Playernames' to stations, in order to
    # compute things like individual participant scores in a mission.
    trial_start_message = key_messages["trial_start"][0]
    client_info = trial_start_message["data"]["client_info"]

    populate_station_to_playername_mapping(
        client_info, db_session, group_session
    )

    return minecraft_mission, minecraft_testbed_messages


def process_directory_v2(group_session: str, db_session):
    """Process directory assuming it contains unified XDF files."""

    minecraft_missions = []
    minecraft_testbed_messages = []
    with cd(f"{group_session}/lsl"):
        streams, header = pyxdf.load_xdf(
            "block_2.xdf", select_streams=[{"type": "minecraft"}]
        )
        stream = streams[0]
        info(f"Processing block_2.xdf for {group_session}.")

        messages = {"Hands-on Training": [], "Saturn_A": [], "Saturn_B": []}
        trial_timestamps = {
            "Hands-on Training": [],
            "Saturn_A": [],
            "Saturn_B": [],
        }

        current_mission = None
        trial_mission = None  # For trial labeling
        testbed_version = None
        for i, timestamp in enumerate(stream["time_stamps"]):
            text = stream["time_series"][i][0]
            try:
                message = json.loads(text)

            except json.decoder.JSONDecodeError:
                topic = text.split()[0][1:-1]
                error_message = f"Unable to parse message number {i + 1} (topic: {topic}) as JSON!"
                if topic in {
                    "agent/ac/belief_diff",
                    "agent/ac/threat_room_coordination",
                }:
                    error_message += (
                        "\n The message is from one of the Rutgers ACs "
                        "(belief difference and threat room coordination),"
                        "which are known to be buggy"
                    )
                else:
                    error_message += f"\n Message text: {text}"

                debug(error_message)
                continue

            topic = message["topic"]

            if topic == "observations/events/mission":
                mission_state = message["data"]["mission_state"]
                if mission_state == "Start":
                    current_mission = message["data"]["mission"]
                    trial_mission = current_mission
                    if messages[current_mission]:
                        info(
                            f"There was already a mission of type {current_mission}"
                            f" so we clear all {len(messages[current_mission])} messages from "
                            f"that mission"
                        )
                        messages[current_mission].clear()
                    messages[current_mission].append((i, message))
                elif mission_state == "Stop":
                    messages[current_mission].append((i, message))
                    current_mission = None
                else:
                    pass
            elif topic == "trial":
                if message["msg"]["sub_type"] == "start":

                    # Get station-to-playername mapping
                    client_info = message["data"]["client_info"]
                    populate_station_to_playername_mapping(
                        client_info, db_session, group_session
                    )

                    trial_start_index = i

                    if testbed_version is None:
                        # The line above assumes that we do not update the testbed in
                        # the middle of a trial.
                        testbed_version = message["data"]["testbed_version"]
                elif message["msg"]["sub_type"] == "stop":
                    trial_stop_index = i
                    trial_timestamps[trial_mission] = (
                        trial_start_index,
                        trial_stop_index,
                    )
                    trial_mission = None
            else:
                if current_mission is not None:
                    messages[current_mission].append((i, message))

        for mission, messages in messages.items():
            print(mission, "\t", len(messages))

            if messages:
                trial_id = messages[0][1]["msg"]["trial_id"]
                assert len(messages) >= 2

                scoreboard_messages = [
                    message[1]
                    for message in messages
                    if message[1]["topic"] == "observations/events/scoreboard"
                ]

                final_team_score = None

                if scoreboard_messages:
                    final_team_score = scoreboard_messages[-1]["data"][
                        "scoreboard"
                    ]["TeamScore"]
                else:
                    error("[MISSING DATA]: No scoreboard messages found!")

                mission_start_timestamp_lsl = stream["time_stamps"][
                    messages[0][0]
                ]
                mission_stop_timestamp_lsl = stream["time_stamps"][
                    messages[-1][0]
                ]

                trial_start_timestamp_lsl = stream["time_stamps"][
                    trial_timestamps[mission][0]
                ]
                trial_stop_timestamp_lsl = stream["time_stamps"][
                    trial_timestamps[mission][1]
                ]

                if trial_id in INVALID_MISSIONS:
                    error(
                        f"[ANOMALY] Skipping {trial_id} as it is a duplicate not removed by the "
                        f"deduplicate logic."
                    )
                else:
                    minecraft_mission = MinecraftMission(
                        group_session_id=group_session,
                        id=trial_id,
                        name=mission,
                        mission_start_timestamp_unix=mission_start_timestamp_lsl,
                        mission_start_timestamp_iso8601=convert_unix_timestamp_to_iso8601(
                            mission_start_timestamp_lsl
                        ),
                        mission_stop_timestamp_unix=mission_stop_timestamp_lsl,
                        mission_stop_timestamp_iso8601=convert_unix_timestamp_to_iso8601(
                            mission_stop_timestamp_lsl
                        ),
                        trial_start_timestamp_unix=trial_start_timestamp_lsl,
                        trial_start_timestamp_iso8601=convert_unix_timestamp_to_iso8601(
                            trial_start_timestamp_lsl
                        ),
                        trial_stop_timestamp_unix=trial_stop_timestamp_lsl,
                        trial_stop_timestamp_iso8601=convert_unix_timestamp_to_iso8601(
                            trial_stop_timestamp_lsl
                        ),
                        final_team_score=final_team_score,
                        testbed_version=testbed_version,
                    )
                    minecraft_missions.append(minecraft_mission)

                    for i, message in messages:
                        minecraft_testbed_messages.append(
                            MinecraftTestbedMessage(
                                mission_id=trial_id,
                                id=i,
                                timestamp_unix=stream["time_stamps"][i],
                                timestamp_iso8601=convert_unix_timestamp_to_iso8601(
                                    stream["time_stamps"][i]
                                ),
                                topic=message.pop("topic"),
                                message=json.dumps(message),
                            )
                        )

    return minecraft_missions, minecraft_testbed_messages


def process_minecraft_data():
    info("Processing directories...")

    with cd(settings.experiment_root_dir):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        db_session = next(get_db())
        processed_group_sessions = set(
            [
                s[0]
                for s in db_session.query(MinecraftMission.group_session_id)
                .distinct(MinecraftMission.group_session_id)
                .all()
            ]
        )

        for group_session in tqdm(
            sorted(directories_to_process), unit="directories"
        ):
            if group_session in processed_group_sessions:
                info(
                    f"Found saved Minecraft data for {group_session} in the database. Skipping "
                    f"group session."
                )
                continue

            info(f"Processing directory {group_session}")

            if not is_directory_with_unified_xdf_files(group_session):
                pass
                missions, messages = process_directory_v1(
                    group_session, db_session
                )
            else:
                missions, messages = process_directory_v2(
                    group_session, db_session
                )

            db_session.add_all(missions)
            # Flush to avoid foreign key error in the messages related to the mission.
            db_session.flush()
            db_session.add_all(messages)
            db_session.commit()
        db_session.close()
