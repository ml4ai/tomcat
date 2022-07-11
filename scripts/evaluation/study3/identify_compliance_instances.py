import argparse
import os
from datetime import datetime, timedelta
from glob import glob

from dateutil.parser import parse
from tqdm import tqdm

from common import metadata_message_generator

TRIAL_TOPIC = "trial"
AGENT_DIALOG_TOPIC = "agent/dialog"
INTERVENTION_TOPIC = "agent/intervention/ASI_UAZ_TA1_ToMCAT/chat"
MINECRAFT_CHAT_TOPIC = "minecraft/chat"

CHECK_UTTERANCE_TIME_WINDOW_SECONDS = 10


class Intervention:
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        self.timestamp = timestamp
        self.for_subject = for_subject

    def __eq__(self, __o: object) -> bool:
        return (
            self.timestamp == __o.timestamp
            and self.for_subject == __o.for_subject
        )


class HelpRequestCriticalVictimIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "help_request_for_critical_victim"
        self.expiration = timestamp + timedelta(
            seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS
        )
        self.compliance_criteria = [
            "CriticalVictim",
            "CriticalMarkerBlock",
            "critical",
        ]


class HelpRequestRoomEscapeIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "help_request_for_room_escape"
        self.expiration = timestamp + timedelta(
            seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS
        )
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
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "help_request_reply"
        self.expiration = timestamp + timedelta(
            seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS
        )
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
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_regular_victim"
        self.expiration = timestamp + timedelta(
            seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS
        )
        self.compliance_criteria = [
            "RegularVictim",
            "RegularMarkerBlock",
            "regular",
        ]


class MarkerBlockCriticalVictimIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_critical_victim"
        self.expiration = timestamp + timedelta(
            seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS
        )
        self.compliance_criteria = [
            "CriticalVictim",
            "CriticalMarkerBlock",
            "critical",
        ]


class MarkerBlockVictimAIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_victim_a"
        self.expiration = timestamp + timedelta(
            seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS
        )
        self.compliance_criteria = [
            "VictimTypeA",
            "TypeAMarker",
            "victim A",
            "A victim",
        ]


class MarkerBlockVictimBIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_victim_b"
        self.expiration = timestamp + timedelta(
            seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS
        )
        self.compliance_criteria = [
            "VictimTypeB",
            "TypeBMarker",
            "victim B",
            "B victim",
        ]


class MarkerBlockRubbleIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_rubble"
        self.expiration = timestamp + timedelta(
            seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS
        )
        self.compliance_criteria = ["Obstacle", "RubbleMarker", "rubble"]


class MarkerBlockNoVictimIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_no_victim"
        self.expiration = timestamp + timedelta(
            seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS
        )
        self.compliance_criteria = [
            "NoVictim",
            "NoVictimMarkerBlock",
            "no victim",
            "empty",
        ]


class MarkerBlockSOSIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_sos"
        self.expiration = timestamp + timedelta(
            seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS
        )
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


def extract_player_information(message) -> dict[str, str]:
    player_information = {}
    for player_data in message["data"]["client_info"]:
        player_information[player_data["playername"]] = player_data[
            "participant_id"
        ]

    return player_information


def extract_intervention(message, timestamp: datetime) -> list[Intervention]:
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
                    timestamp, receiver
                )
                interventions.append(intervention)
        if "threat room" in explanation:
            for receiver in message["data"]["receivers"]:
                intervention = HelpRequestRoomEscapeIntervention(
                    timestamp, receiver
                )
                interventions.append(intervention)
    elif "did not get an answer" in explanation:
        for receiver in message["data"]["receivers"]:
            intervention = HelpRequestReplyIntervention(timestamp, receiver)
            interventions.append(intervention)
    elif (
        "placed a marker" in explanation and "regular victim marker" in content
    ):
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockRegularVictimIntervention(
                timestamp, receiver
            )
            interventions.append(intervention)
    elif (
        "placed a marker" in explanation
        and "critical victim marker" in content
    ):
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockCriticalVictimIntervention(
                timestamp, receiver
            )
            interventions.append(intervention)
    elif "placed a marker" in explanation and "A marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockVictimAIntervention(timestamp, receiver)
            interventions.append(intervention)
    elif "placed a marker" in explanation and "B marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockVictimBIntervention(timestamp, receiver)
            interventions.append(intervention)
    elif "placed a marker" in explanation and "rubble marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockRubbleIntervention(timestamp, receiver)
            interventions.append(intervention)
    elif "placed a marker" in explanation and "no victim marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockNoVictimIntervention(timestamp, receiver)
            interventions.append(intervention)
    elif "placed a marker" in explanation and "sos marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockSOSIntervention(timestamp, receiver)
            interventions.append(intervention)
    else:
        print(
            "[INFO] Event is ignored. Explanation: "
            + message["data"]["explanation"]["info"]
            + " Content: "
            + message["data"]["content"]
        )

    return interventions


def log_report(output_dir: str, report: dict) -> None:
    with open(output_dir + "/compliance_instances_report.txt", "w") as file:
        intervention_compliance_count = {}

        for file_name, file_interventions in report.items():
            file.write(
                "--------------------------------------------------------------\n"
            )
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
                    if intervention["compliance"] is not None:
                        num_compliances_for_intervention_type += 1

                intervention_compliance_count[intervention_type][
                    "num_compliances"
                ] += num_compliances_for_intervention_type

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

            file.write("\n")

        file.write(
            "--------------------------------------------------------------\n"
        )

        total_num_interventions = 0
        total_num_compliances = 0
        for (
            intervention_type,
            interventions,
        ) in intervention_compliance_count.items():
            file.write(
                f"{intervention_type}: total "
                + str(interventions["num_interventions"])
                + " interventions, "
                + str(interventions["num_compliances"])
                + " compliances.\n"
            )

            total_num_interventions += interventions["num_interventions"]
            total_num_compliances += interventions["num_compliances"]

        file.write(f"Number of interventions: {total_num_interventions}\n")
        file.write(f"Number of compliances: {total_num_compliances}\n")
        file.write(
            "--------------------------------------------------------------\n"
        )


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
    args = parser.parse_args()

    report = {}

    for filepath in glob(args.data_dir + "/*T00*UAZ*.metadata"):
        metadata_file_name = os.path.basename(filepath)
        report[metadata_file_name] = {}

        player_information: dict[str, str] = {}
        watch_interventions: list[Intervention] = []

        # sort messages in the metadata
        print("Sorting messages in file " + metadata_file_name)
        messages = []
        for message in metadata_message_generator(filepath):
            messages.append(message)

        sorted_messages = sorted(
            messages, key=lambda x: parse(x["msg"]["timestamp"])
        )

        trial_started = False

        # parse messages
        total_num_messages = len(sorted_messages)
        pbar = tqdm(total=total_num_messages)
        for message in sorted_messages:
            pbar.update()

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
                    player_information = extract_player_information(message)
                    trial_started = True
                    continue
                # end parsing after the trial has ended
                else:
                    pbar.n = total_num_messages
                    pbar.close()
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

                interventions = extract_intervention(message, timestamp)

                for intervention in interventions:
                    watch_interventions.append(intervention)

                    if intervention.type not in report[metadata_file_name]:
                        report[metadata_file_name][intervention.type] = {}

                    report[metadata_file_name][intervention.type][
                        intervention.timestamp
                    ] = {
                        "for_subject": intervention.for_subject,
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
                                    report[metadata_file_name][
                                        intervention.type
                                    ][intervention.timestamp]["compliance"] = {
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
                                report[metadata_file_name][intervention.type][
                                    intervention.timestamp
                                ]["compliance"] = {
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
                            report[metadata_file_name][intervention.type][
                                intervention.timestamp
                            ]["compliance"] = {
                                "timestamp": timestamp,
                                "reason": f"minecraft chat text matchs compliance tag: {compliance_tag}",
                            }

                for intervention in complied_interventions:
                    watch_interventions.remove(intervention)

    log_report(args.output_dir, report)
