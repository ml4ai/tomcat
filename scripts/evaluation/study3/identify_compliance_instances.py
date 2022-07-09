import argparse
from datetime import datetime, timedelta
from enum import Enum
from glob import glob

from dateutil.parser import parse

from common import metadata_message_generator

TRIAL_TOPIC = "trial"
AGENT_DIALOG_TOPIC = "agent/dialog"
INTERVENTION_TOPIC = "agent/intervention/ASI_UAZ_TA1_ToMCAT/chat"


CHECK_UTTERANCE_TIME_WINDOW_SECONDS = 20


class InterventionType(Enum):
    UTTERANCE = 1
    MARKER_PLACEMENT = 2


class Intervention:
    def __init__(self,
                 timestamp: datetime,
                 for_player: str) -> None:
        self.timestamp = timestamp
        self.for_player = for_player

    def __eq__(self, __o: object) -> bool:
        return self.timestamp == __o.timestamp and self.for_player == __o.for_player


class HelpRequestCritcalVictimIntervention(Intervention):
    def __init__(self,
                 timestamp: datetime,
                 for_player: str) -> None:
        super().__init__(timestamp, for_player)
        self.type = InterventionType.UTTERANCE
        self.description = "help_request_for_critical_victim"
        self.expiration = timestamp + \
            timedelta(seconds=CHECK_UTTERANCE_TIME_WINDOW_SECONDS)
        self.compliance_criteria = ["CriticalVictim",
                                    "CriticalMarkerBlock"]


def extract_player_information(message) -> dict[str, str]:
    player_information = {}
    for player_data in message["data"]["client_info"]:
        player_information[player_data["playername"]
                           ] = player_data["participant_id"]

    return player_information


def extract_intervention(message, timestamp: datetime) -> list[Intervention]:
    explanation = message["data"]["explanation"]["info"].replace(
        "This intervention was triggered ", "")

    interventions = []

    if "did not ask" in explanation:
        if "critical victim" in explanation:
            for receiver in message["data"]["receivers"]:
                intervention = HelpRequestCritcalVictimIntervention(
                    timestamp, receiver)
                interventions.append(intervention)
    else:
        print("[INFO] Event is ignored: " +
              message["data"]["explanation"]["info"])

    return interventions


def log_complied_intervention(intervention: Intervention) -> None:
    print(f"[INFO] Intervention {intervention.description} succeeded by player {intervention.for_player}")


if __name__ == "__main__":
    # parsing program arguments
    parser = argparse.ArgumentParser(
        description="Evaluate compliance in trials")
    parser.add_argument(
        "--data_dir",
        help="Directory containing .metadata files",
        default="/media/mule/projects/tomcat/protected/study-3_2022",
    )
    args = parser.parse_args()

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
                watch_interventions += interventions

            # parse dialog agent message to check for compliance
            if "topic" in message and message["topic"] == AGENT_DIALOG_TOPIC:
                if message["data"]["participant_id"] == "Server":
                    continue

                # ensure player identification consistency
                assert message["data"]["participant_id"] in player_information.keys()

                complied_interventions = []
                for labels in message["data"]["extractions"]:
                    for intervention in watch_interventions:
                        if intervention.for_player == player_information[message["data"]["participant_id"]]:
                            if intervention.type == InterventionType.UTTERANCE:
                                for compliance_label in intervention.compliance_criteria:
                                    if compliance_label in labels and intervention not in complied_interventions:
                                        complied_interventions.append(intervention)

                for intervention in complied_interventions:
                    log_complied_intervention(intervention)
                    watch_interventions.remove(intervention)
