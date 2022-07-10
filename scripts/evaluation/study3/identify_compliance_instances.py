import argparse
from datetime import datetime, timedelta
from glob import glob

from dateutil.parser import parse

from common import metadata_message_generator

TRIAL_TOPIC = "trial"
AGENT_DIALOG_TOPIC = "agent/dialog"
INTERVENTION_TOPIC = "agent/intervention/ASI_UAZ_TA1_ToMCAT/chat"


CHECK_UTTERANCE_TIME_WINDOW_SECONDS = 10


class Intervention:
    def __init__(self,
                 timestamp: datetime,
                 for_subject: str) -> None:
        self.timestamp = timestamp
        self.for_subject = for_subject

    def __eq__(self, __o: object) -> bool:
        return self.timestamp == __o.timestamp and self.for_subject == __o.for_subject


class HelpRequestCritcalVictimIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "help_request_for_critical_victim"
        self.expiration = timestamp + \
            timedelta(seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS)
        self.compliance_criteria = ["CriticalVictim",
                                    "CriticalMarkerBlock",
                                    "critical"]


class HelpRequestRoomEscapeIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "help_request_for_room_escape"
        self.expiration = timestamp + \
            timedelta(seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS)
        self.compliance_criteria = ["Stuck",
                                    "HelpRequest",
                                    "NeedAction",
                                    "NeedItem",
                                    "NeedRole",
                                    "SOSMarker"]


class HelpRequestReplyIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "help_request_reply"
        self.expiration = timestamp + \
            timedelta(seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS)
        self.compliance_criteria = ["Stuck",
                                    "HelpRequest",
                                    "NeedAction",
                                    "NeedItem",
                                    "NeedRole",
                                    "SOSMarker"]


class MarkerBlockRegularVictimIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_regular_victim"
        self.expiration = timestamp + \
            timedelta(seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS)
        self.compliance_criteria = ["RegularVictim",
                                    "RegularMarkerBlock",
                                    "regular"]


class MarkerBlockCriticalVictimIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_critical_victim"
        self.expiration = timestamp + \
            timedelta(seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS)
        self.compliance_criteria = ["CriticalVictim",
                                    "CriticalMarkerBlock",
                                    "critical"]


class MarkerBlockVictimAIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_victim_a"
        self.expiration = timestamp + \
            timedelta(seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS)
        self.compliance_criteria = ["VictimTypeA",
                                    "TypeAMarker"]


class MarkerBlockVictimBIntervention(Intervention):
    def __init__(self, timestamp: datetime, for_subject: str) -> None:
        super().__init__(timestamp, for_subject)
        self.type = "marker_block_victim_b"
        self.expiration = timestamp + \
            timedelta(seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS)
        self.compliance_criteria = ["VictimTypeB",
                                    "TypeBMarker"]


def extract_player_information(message) -> dict[str, str]:
    player_information = {}
    for player_data in message["data"]["client_info"]:
        player_information[player_data["playername"]
                           ] = player_data["participant_id"]

    return player_information


def extract_intervention(message, timestamp: datetime) -> list[Intervention]:
    explanation = message["data"]["explanation"]["info"].replace(
        "This intervention was triggered ", "")
    content = message["data"]["content"]

    interventions = []

    if "to ensure" in explanation:
        return interventions
    elif "did not ask" in explanation:
        if "critical victim" in explanation:
            for receiver in message["data"]["receivers"]:
                intervention = HelpRequestCritcalVictimIntervention(
                    timestamp, receiver)
                interventions.append(intervention)
        if "threat room" in explanation:
            for receiver in message["data"]["receivers"]:
                intervention = HelpRequestRoomEscapeIntervention(
                    timestamp, receiver)
                interventions.append(intervention)
    elif "did not get an answer" in explanation:
        for receiver in message["data"]["receivers"]:
            intervention = HelpRequestReplyIntervention(timestamp, receiver)
            interventions.append(intervention)
    elif "placed a marker" in explanation and "regular victim marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockRegularVictimIntervention(
                timestamp, receiver)
            interventions.append(intervention)
    elif "placed a marker" in explanation and "critical victim marker" in content:
        for receiver in message["data"]["receivers"]:
            intervention = MarkerBlockCriticalVictimIntervention(
                timestamp, receiver)
            interventions.append(intervention)
    else:
        print("[INFO] Event is ignored: " +
              message["data"]["explanation"]["info"])

    return interventions


def log_report(output_dir: str, report: dict) -> None:
    with open(output_dir + "/compliance_instances_report.txt", 'w') as file:
        num_interventions = 0
        num_compliances = 0

        for intervention_type, interventions in report.items():
            num_interventions += len(interventions)

            file.write(f"{intervention_type}: {len(interventions)}\n")

            for timestamp, intervention in interventions.items():
                if intervention["compliance"] is not None:
                    num_compliances += 1

                    file.write(timestamp.isoformat() +
                               " for subject " +
                               intervention["for_subject"] +
                               ", " +
                               intervention["compliance"]["timestamp"].isoformat() +
                               ' ' +
                               intervention["compliance"]["reason"] +
                               '\n')

        file.write(f"Number of interventions: {num_interventions}\n")
        file.write(f"Number of compliances: {num_compliances}\n")


if __name__ == "__main__":
    # parsing program arguments
    parser = argparse.ArgumentParser(
        description="Evaluate compliance in trials")
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
        player_information: dict[str, str] = {}
        watch_interventions: list[Intervention] = []

        trial_started = False

        for message in metadata_message_generator(filepath):
            timestamp = parse(message["msg"]["timestamp"])

            # resolve expired interventions
            watch_interventions = list(filter(
                lambda intervention: intervention.expiration >= timestamp,
                watch_interventions
            ))

            # parse trial message
            if "topic" in message and message["topic"] == TRIAL_TOPIC:
                # extract player information
                if message["msg"]["sub_type"] == "start":
                    player_information = extract_player_information(message)
                    trial_started = True
                    continue
                # end parsing after the trial has ended
                else:
                    break
            # only start monitoring after trial has started
            elif not trial_started:
                continue

            # parse ToMCAT intervention message
            if "topic" in message and message["topic"] == INTERVENTION_TOPIC:
                # ensure player identification consistency
                assert set(message["data"]["receivers"]).issubset(
                    player_information.values())

                interventions = extract_intervention(message, timestamp)

                for intervention in interventions:
                    watch_interventions.append(intervention)

                    if intervention.type not in report:
                        report[intervention.type] = {}

                    report[intervention.type][intervention.timestamp] = {
                        "for_subject": intervention.for_subject,
                        "compliance": None
                    }

            # parse dialog agent message to check for compliance
            if "topic" in message and message["topic"] == AGENT_DIALOG_TOPIC:
                if message["data"]["participant_id"] == "Server":
                    continue

                # ensure player identification consistency
                assert message["data"]["participant_id"] in player_information.keys()

                # check for any intervention that has been complied by the subject
                complied_interventions = []
                for intervention in watch_interventions:
                    compliance_match_reason = None

                    if player_information[message["data"]["participant_id"]] == intervention.for_subject:
                        # check if the utterance label matches the compliance criteria
                        for compliance_tag in intervention.compliance_criteria:
                            for labels in message["data"]["extractions"]:
                                if compliance_tag in labels:
                                    complied_interventions.append(intervention)
                                    compliance_match_reason = f"utterance label matchs compliance tag: {compliance_tag}"
                                    break
                            if compliance_match_reason is not None:
                                break
                        # check if the utterance text contains any word that matches the compliance criteria
                        else:
                            if compliance_tag in message["data"]["text"]:
                                complied_interventions.append(intervention)
                                compliance_match_reason = f"utterance text matchs compliance tag: {compliance_tag}"

                for intervention in complied_interventions:
                    watch_interventions.remove(intervention)

                    report[intervention.type][intervention.timestamp]["compliance"] = {
                        "timestamp": timestamp,
                        "reason": compliance_match_reason
                    }

    log_report(args.output_dir, report)
