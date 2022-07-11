#!/usr/bin/env python

import argparse
import os
from datetime import datetime, timedelta
from glob import glob
from typing import Dict, List, Tuple

import pandas as pd
from dateutil.parser import parse
from tqdm import tqdm

from common import metadata_message_generator

TRIAL_TOPIC = "trial"
AGENT_DIALOG_TOPIC = "agent/dialog"
INTERVENTION_TOPIC = "agent/intervention/ASI_UAZ_TA1_ToMCAT/chat"
MINECRAFT_CHAT_TOPIC = "minecraft/chat"

DEFAULT_CHECK_UTTERANCE_TIME_WINDOW_SECONDS = 10


class Intervention:
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        self.timestamp = timestamp
        self.for_subject = for_subject
        self.expiration = timestamp + timedelta(seconds=seconds_window)

    def __eq__(self, __o: object) -> bool:
        return (
            self.timestamp == __o.timestamp
            and self.for_subject == __o.for_subject
        )


class HelpRequestCriticalVictimIntervention(Intervention):
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        super().__init__(timestamp, for_subject, seconds_window)
        self.type = "help_request_for_critical_victim"
        self.compliance_criteria = [
            "CriticalVictim",
            "CriticalMarkerBlock",
            "critical",
        ]


class HelpRequestRoomEscapeIntervention(Intervention):
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        super().__init__(timestamp, for_subject, seconds_window)
        self.type = "help_request_for_room_escape"
        self.compliance_criteria = [
            "Stuck",
            "HelpRequest",
            "NeedAction",
            "NeedItem",
            "NeedRole",
            "SOSMarker",
            "help",
            "stuck",
        ]


class HelpRequestReplyIntervention(Intervention):
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        super().__init__(timestamp, for_subject, seconds_window)
        self.type = "help_request_reply"
        self.compliance_criteria = [
            "Stuck",
            "HelpRequest",
            "NeedAction",
            "NeedItem",
            "NeedRole",
            "SOSMarker",
            "help",
            "stuck",
        ]


class MarkerBlockRegularVictimIntervention(Intervention):
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        super().__init__(timestamp, for_subject, seconds_window)
        self.type = "marker_block_regular_victim"
        self.compliance_criteria = [
            "RegularVictim",
            "RegularMarkerBlock",
            "regular",
        ]


class MarkerBlockCriticalVictimIntervention(Intervention):
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        super().__init__(timestamp, for_subject, seconds_window)
        self.type = "marker_block_critical_victim"
        self.compliance_criteria = [
            "CriticalVictim",
            "CriticalMarkerBlock",
            "critical",
        ]


class MarkerBlockVictimAIntervention(Intervention):
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        super().__init__(timestamp, for_subject, seconds_window)
        self.type = "marker_block_victim_a"
        self.compliance_criteria = [
            "VictimTypeA",
            "TypeAMarker",
            "victim A",
            "A victim",
        ]


class MarkerBlockVictimBIntervention(Intervention):
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        super().__init__(timestamp, for_subject, seconds_window)
        self.type = "marker_block_victim_b"
        self.compliance_criteria = [
            "VictimTypeB",
            "TypeBMarker",
            "victim B",
            "B victim",
        ]


class MarkerBlockRubbleIntervention(Intervention):
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        super().__init__(timestamp, for_subject, seconds_window)
        self.type = "marker_block_rubble"
        self.compliance_criteria = [
            "Obstacle",
            "RubbleMarker",
            "rubble"
        ]


class MarkerBlockNoVictimIntervention(Intervention):
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        super().__init__(timestamp, for_subject, seconds_window)
        self.type = "marker_block_no_victim"
        self.compliance_criteria = [
            "NoVictim",
            "NoVictimMarkerBlock",
            "no victim",
            "empty",
        ]


class MarkerBlockSOSIntervention(Intervention):
    def __init__(
        self, timestamp: datetime, for_subject: str, seconds_window: int
    ) -> None:
        super().__init__(timestamp, for_subject, seconds_window)
        self.type = "marker_block_sos"
        self.compliance_criteria = [
            "Stuck",
            "HelpRequest",
            "NeedAction",
            "NeedItem",
            "NeedRole",
            "SOSMarker",
            "help",
            "stuck",
        ]


def extract_player_information(
    message,
) -> Tuple[Dict[str, str], Dict[str, str]]:
    player_information = {}
    player_callsign = {}
    for player_data in message["data"]["client_info"]:
        player_information[player_data["playername"]] = player_data[
            "participant_id"
        ]
        player_callsign[player_data["participant_id"]] = player_data[
            "callsign"
        ]

    return player_information, player_callsign


def extract_intervention(
    message, timestamp: datetime, seconds_window: int
) -> List[Intervention]:
    explanation = message["data"]["explanation"]["info"].replace(
        "This intervention was triggered ", ""
    )
    content = message["data"]["content"]

    interventions = []

    if "to ensure" in explanation:
        return interventions
    elif "did not ask" in explanation:
        if "critical victim" in explanation:
            for receiver in message["data"]["receivers"]:
                intervention = HelpRequestCriticalVictimIntervention(
                    timestamp, receiver, seconds_window
                )
                interventions.append(intervention)
        if "threat room" in explanation:
            for receiver in message["data"]["receivers"]:
                intervention = HelpRequestRoomEscapeIntervention(
                    timestamp, receiver, seconds_window
                )
                interventions.append(intervention)
    elif "did not get an answer" in explanation:
        for receiver in message["data"]["receivers"]:
            intervention = HelpRequestReplyIntervention(
                timestamp, receiver, seconds_window
            )
            interventions.append(intervention)
    elif (
        "placed a marker" in explanation and "regular victim marker" in content
    ):
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockRegularVictimIntervention(
                timestamp, receiver, seconds_window
            )
            interventions.append(intervention)
    elif (
        "placed a marker" in explanation
        and "critical victim marker" in content
    ):
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockCriticalVictimIntervention(
                timestamp, receiver, seconds_window
            )
            interventions.append(intervention)
    elif "placed a marker" in explanation and "A marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockVictimAIntervention(
                timestamp, receiver, seconds_window
            )
            interventions.append(intervention)
    elif "placed a marker" in explanation and "B marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockVictimBIntervention(
                timestamp, receiver, seconds_window
            )
            interventions.append(intervention)
    elif "placed a marker" in explanation and "rubble marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockRubbleIntervention(
                timestamp, receiver, seconds_window
            )
            interventions.append(intervention)
    elif "placed a marker" in explanation and "no victim marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockNoVictimIntervention(
                timestamp, receiver, seconds_window
            )
            interventions.append(intervention)
    elif "placed a marker" in explanation and "sos marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockSOSIntervention(
                timestamp, receiver, seconds_window
            )
            interventions.append(intervention)
    else:
        print(
            "[INFO] Event is ignored. Explanation: "
            + message["data"]["explanation"]["info"]
            + " Content: "
            + message["data"]["content"]
        )

    return interventions


def report_compliance(
    output_dir: str, report: Dict, report_specifics: bool = False
) -> None:
    num_seconds_window = report["seconds_window"]

    with open(
        output_dir + f"/compliance_report_{num_seconds_window}_seconds.md", "w"
    ) as file:
        intervention_compliance_count = {}
        player_compliance_count = {}

        for file_name, file_interventions in report["compliance_data"].items():
            if report_specifics:
                file.write(file_name + "\n")
                file.write(
                    "--------------------------------------------------------------\n\n"
                )

            for intervention_type, interventions in file_interventions.items():
                if intervention_type not in intervention_compliance_count:
                    intervention_compliance_count[intervention_type] = {
                        "num_interventions": 0,
                        "num_compliances": 0,
                    }

                intervention_compliance_count[intervention_type][
                    "num_interventions"
                ] += len(interventions)

                # count number of compliances for this intervention type
                num_compliances_for_intervention_type = 0
                for intervention in interventions.values():
                    if (
                        intervention["subject_callsign"]
                        not in player_compliance_count
                    ):
                        player_compliance_count[
                            intervention["subject_callsign"]
                        ] = {}

                    if (
                        intervention_type
                        not in player_compliance_count[
                            intervention["subject_callsign"]
                        ]
                    ):
                        player_compliance_count[
                            intervention["subject_callsign"]
                        ][intervention_type] = {
                            "num_interventions": 0,
                            "num_compliances": 0,
                        }

                    player_compliance_count[intervention["subject_callsign"]][
                        intervention_type
                    ]["num_interventions"] += 1

                    if intervention["compliance"] is not None:
                        player_compliance_count[
                            intervention["subject_callsign"]
                        ][intervention_type]["num_compliances"] += 1

                        num_compliances_for_intervention_type += 1

                intervention_compliance_count[intervention_type][
                    "num_compliances"
                ] += num_compliances_for_intervention_type

                if (
                    intervention_compliance_count[intervention_type][
                        "num_interventions"
                    ]
                    > 0
                ):
                    intervention_compliance_count[intervention_type][
                        "compliance_statistics (%)"
                    ] = (
                        100
                        * float(
                            intervention_compliance_count[intervention_type][
                                "num_compliances"
                            ]
                        )
                        / float(
                            intervention_compliance_count[intervention_type][
                                "num_interventions"
                            ]
                        )
                    )
                else:
                    intervention_compliance_count[intervention_type][
                        "compliance_statistics (%)"
                    ] = 0.0

                if report_specifics:
                    file.write(
                        f"{intervention_type}: {len(interventions)}, with {num_compliances_for_intervention_type} compliances\n"
                    )

                    # report compliances
                    for timestamp, intervention in interventions.items():
                        if intervention["compliance"] is not None:
                            file.write(
                                timestamp.isoformat()
                                + " for subject "
                                + intervention["for_subject"]
                                + ", "
                                + intervention["compliance"][
                                    "timestamp"
                                ].isoformat()
                                + " "
                                + intervention["compliance"]["reason"]
                                + "\n"
                            )

                    file.write("\n")

            if report_specifics:
                file.write("\n")

        file.write(
            "Compliance by intervention types\n"
            "--------------------------------\n\n"
        )

        df = pd.DataFrame(data=intervention_compliance_count).T
        file.write(df.to_markdown())
        file.write("\n\n")

        file.write("Compliance by player\n" "--------------------\n\n")

        for callsign, interventions in player_compliance_count.items():
            for intervention_type, intervention_count in interventions.items():
                if intervention_count["num_interventions"] > 0:
                    player_compliance_count[callsign][intervention_type][
                        "compliance_statistics (%)"
                    ] = (
                        100
                        * float(intervention_count["num_compliances"])
                        / float(intervention_count["num_interventions"])
                    )
                else:
                    player_compliance_count[callsign][intervention_type][
                        "compliance_statistics (%)"
                    ] = 0.0

            df_player = pd.DataFrame(data=interventions).T

            player_num_interventions = df_player["num_interventions"].sum()
            player_num_compliances = df_player["num_compliances"].sum()

            if player_num_interventions > 0:
                player_compliance_percentage = float(
                    player_num_compliances
                ) / float(player_num_interventions)
            else:
                player_compliance_percentage = 0.0

            file.write("Player callsign: " + callsign + "\n")
            file.write(
                f"Interventions for {callsign}: {player_num_interventions}\n"
            )
            file.write(
                f"Overall {callsign} compliances: {player_num_compliances} / {player_num_interventions} "
            )
            file.write("({:.2%})\n\n".format(player_compliance_percentage))

            file.write(df_player.to_markdown())
            file.write("\n\n")

        file.write("Overall statistics\n" "------------------\n\n")

        num_interventions = df["num_interventions"].sum()
        num_compliances = df["num_compliances"].sum()

        if num_interventions > 0:
            compliance_percentage = float(num_compliances) / float(
                num_interventions
            )
        else:
            compliance_percentage = 0.0

        file.write(
            "Compliance check window: "
            + str(num_seconds_window)
            + " seconds\n"
        )
        file.write(f"Interventions considered: {num_interventions}\n")
        file.write(
            f"Overall compliances: {num_compliances} / {num_interventions} "
        )
        file.write("({:.2%})\n".format(compliance_percentage))


if __name__ == "__main__":
    # parsing program arguments
    parser = argparse.ArgumentParser(
        description="Evaluate compliance in trials"
    )
    parser.add_argument(
        "--data_dir",
        help="Directory containing .metadata files",
        default="/media/mule/projects/tomcat/protected/study-3_2022",
    )
    parser.add_argument(
        "--output_dir",
        help="Output directory",
        default=".",
    )
    parser.add_argument(
        "--sort",
        help="Sort messages in metadata file",
        action="store_true",
        default=False,
    )
    parser.add_argument(
        "--seconds",
        type=int,
        help="Number of seconds to check for intervention compliance",
        default=DEFAULT_CHECK_UTTERANCE_TIME_WINDOW_SECONDS,
    )
    parser.add_argument(
        "--report_specifics",
        help="Report specifics about compliance instances",
        action="store_true",
        default=False,
    )
    args = parser.parse_args()

    report = {}
    report["seconds_window"] = args.seconds
    report["compliance_data"] = {}

    for filepath in tqdm(
        glob(args.data_dir + "/HSRData*TrialMessages_Trial-T00*UAZ*.metadata")
    ):
        metadata_file_name = os.path.basename(filepath)
        report["compliance_data"][metadata_file_name] = {}

        player_information: Dict[str, str] = {}
        player_callsign: Dict[str, str] = {}
        watch_interventions: List[Intervention] = []

        # sort messages in the metadata
        if args.sort:
            print("Sorting messages in file " + metadata_file_name)
            json_messages = []
            for message in metadata_message_generator(filepath):
                json_messages.append(message)

            messages = sorted(
                json_messages, key=lambda x: parse(x["msg"]["timestamp"])
            )
        else:
            messages = metadata_message_generator(filepath)

        trial_started = False

        # parse messages
        for message in messages:
            timestamp = parse(message["msg"]["timestamp"])

            # resolve expired interventions
            watch_interventions = list(
                filter(
                    lambda intervention: intervention.expiration >= timestamp,
                    watch_interventions,
                )
            )

            # parse trial message
            if "topic" in message and message["topic"] == TRIAL_TOPIC:
                # extract player information
                if message["msg"]["sub_type"] == "start":
                    (
                        player_information,
                        player_callsign,
                    ) = extract_player_information(message)
                    trial_started = True
                    continue
                # end parsing after the trial has ended
                else:
                    break

            # only start monitoring after trial has started
            elif not trial_started:
                continue

            # parse ToMCAT intervention message
            elif "topic" in message and message["topic"] == INTERVENTION_TOPIC:
                # ensure player identification consistency
                assert set(message["data"]["receivers"]).issubset(
                    player_information.values()
                )

                interventions = extract_intervention(
                    message, timestamp, args.seconds
                )

                for intervention in interventions:
                    watch_interventions.append(intervention)

                    if (
                        intervention.type
                        not in report["compliance_data"][metadata_file_name]
                    ):
                        report["compliance_data"][metadata_file_name][
                            intervention.type
                        ] = {}

                    report["compliance_data"][metadata_file_name][
                        intervention.type
                    ][intervention.timestamp] = {
                        "for_subject": intervention.for_subject,
                        "subject_callsign": player_callsign[
                            intervention.for_subject
                        ],
                        "compliance": None,
                    }

            # parse dialog agent message to check for compliance
            elif (
                "topic" in message
                and message["topic"] == AGENT_DIALOG_TOPIC
                and message["data"]["participant_id"] != "Server"
            ):
                # check for any intervention that has been complied by the subject
                complied_interventions = []
                for intervention in watch_interventions:
                    if (
                        player_information[message["data"]["participant_id"]]
                        == intervention.for_subject
                    ):
                        compliance_found = False

                        # check if the utterance label matches the compliance criteria
                        for compliance_tag in intervention.compliance_criteria:
                            for labels in message["data"]["extractions"]:
                                if compliance_tag in labels["labels"]:
                                    complied_interventions.append(intervention)

                                    # record compliance information
                                    report["compliance_data"][
                                        metadata_file_name
                                    ][intervention.type][
                                        intervention.timestamp
                                    ][
                                        "compliance"
                                    ] = {
                                        "timestamp": timestamp,
                                        "reason": f"utterance label matchs compliance tag: {compliance_tag}",
                                    }

                                    compliance_found = True
                                    break
                            if compliance_found:
                                break

                        # check if the utterance text contains any word that matches the compliance criteria
                        else:
                            if compliance_tag in message["data"]["text"]:
                                complied_interventions.append(intervention)

                                # record compliance information
                                report["compliance_data"][metadata_file_name][
                                    intervention.type
                                ][intervention.timestamp]["compliance"] = {
                                    "timestamp": timestamp,
                                    "reason": f"utterance text matchs compliance tag: {compliance_tag}",
                                }

                # remove interventions that are complied from the watch list
                for intervention in complied_interventions:
                    watch_interventions.remove(intervention)

            # parse minecraft chat messages to check for compliance
            elif (
                "topic" in message
                and message["topic"] == MINECRAFT_CHAT_TOPIC
                and message["data"]["sender"] != "Server"
            ):
                # check for any intervention that has been complied by the subject
                complied_interventions = []
                for intervention in watch_interventions:
                    if (
                        player_information[message["data"]["sender"]]
                        == intervention.for_subject
                    ):
                        # check if the utterance text contains any word that matches the compliance criteria
                        if compliance_tag in message["data"]["text"]:
                            complied_interventions.append(intervention)

                            # record compliance information
                            report["compliance_data"][metadata_file_name][
                                intervention.type
                            ][intervention.timestamp]["compliance"] = {
                                "timestamp": timestamp,
                                "reason": f"minecraft chat text matchs compliance tag: {compliance_tag}",
                            }

                for intervention in complied_interventions:
                    watch_interventions.remove(intervention)

    report_compliance(args.output_dir, report, args.report_specifics)
