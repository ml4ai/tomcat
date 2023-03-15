from __future__ import annotations

from typing import Any, Dict, Optional

from datetime import datetime
from dateutil.parser import parse
from glob import glob
import json
import logging
import pandas as pd

from summary.task_summary import TaskSummary
from common.constants import MISSING_INFO

logger = logging.getLogger()


class USARMissionSummary(TaskSummary):
    TRIAL_TOPIC = "trial"
    ASR_TOPIC = "agent/asr/final"
    DIALOG_TOPIC = "agent/dialog"
    SCOREBOARD_TOPIC = "observations/events/scoreboard"
    MISSION_STATE_TOPIC = "observations/events/mission"

    def __init__(self):
        super().__init__("Mission")

        self.trial_id = MISSING_INFO
        self.trial_number = MISSING_INFO
        self.map_name = MISSING_INFO
        self.trial_start: Optional[datetime] = None
        self.trial_end: Optional[datetime] = None
        self.mission_start: Optional[datetime] = None
        self.mission_end: Optional[datetime] = None
        self.team_score = 0
        self.num_messages = 0
        self.num_asr_messages = 0
        self.num_dialog_messages = 0
        self.intervention_agents = MISSING_INFO

    @property
    def trial_duration(self) -> str:
        if self.trial_start is None or self.trial_end is None:
            return MISSING_INFO
        else:
            return str((self.trial_end - self.trial_start).total_seconds())

    @property
    def mission_duration(self) -> str:
        if self.mission_start is None or self.mission_end is None:
            return MISSING_INFO
        else:
            return str((self.mission_end - self.mission_start).total_seconds())

    @classmethod
    def from_metadata_file(cls, metadata_filepath: str) -> USARMissionSummary:
        messages = []
        with open(metadata_filepath, 'r') as f:
            for line in f:
                try:
                    json_message = json.loads(line)
                    messages.append(json_message)
                except:
                    logger.error(f"Bad json line of len: {len(line)}, {line}")

        # Paulo Soares:
        # At this point, we don't need to process the messages in chronological order. I am commenting this line
        # to speed up the process. Consider uncommenting it in the future if some piece of information needs to be
        # processed as they happened.
        # messages = sorted(
        #     messages, key=lambda x: parse(x["header"]["timestamp"])
        # )

        mission_summary = cls()
        for json_message in messages:
            topic = json_message.get("topic", "")

            if topic == USARMissionSummary.TRIAL_TOPIC:
                USARMissionSummary._parse_trial_info(json_message, mission_summary)
            elif topic == USARMissionSummary.MISSION_STATE_TOPIC:
                USARMissionSummary._parse_mission_state(json_message, mission_summary)
            elif topic == USARMissionSummary.SCOREBOARD_TOPIC:
                USARMissionSummary._parse_team_score(json_message, mission_summary)
            elif topic == USARMissionSummary.ASR_TOPIC:
                mission_summary.num_asr_messages += 1
            elif topic == USARMissionSummary.DIALOG_TOPIC:
                mission_summary.num_dialog_messages += 1

            mission_summary.num_messages += 1

        return mission_summary

    @staticmethod
    def _parse_trial_info(json_message: Any, mission_summary: USARMissionSummary) -> None:
        if json_message["msg"]["sub_type"] == "start":
            mission_summary.trial_start = parse(json_message["msg"]["timestamp"])
            mission_summary.trial_id = json_message["msg"]["trial_id"]
            mission_summary.trial_number = json_message["data"]["trial_number"]

            map_filename = str.lower(json_message["data"]["map_block_filename"])
            if "training" in map_filename:
                mission_summary.map_name = "training"
            elif "saturna" in map_filename:
                mission_summary.map_name = "saturn_a"
            elif "saturnb" in map_filename:
                mission_summary.map_name = "saturn_b"
            else:
                mission_summary.map_name = map_filename

            mission_summary.intervention_agents = ",".join(json_message["data"]["intervention_agents"])
        else:
            mission_summary.trial_end = parse(json_message["msg"]["timestamp"])

    @staticmethod
    def _parse_mission_state(json_message: Any, mission_summary: USARMissionSummary) -> None:
        state = json_message["data"]["mission_state"].lower()
        if state == "start":
            mission_summary.mission_start = parse(json_message["header"]["timestamp"])
        else:
            mission_summary.mission_end = parse(json_message["header"]["timestamp"])

    @staticmethod
    def _parse_team_score(json_message: Any, mission_summary: USARMissionSummary):
        new_score = int(json_message["data"]["scoreboard"]["TeamScore"])
        # Take the maximum in case the messages are not processed in chronological order
        mission_summary.team_score = max(mission_summary.team_score, new_score)

    def is_training(self) -> bool:
        return self.map_name == "training"

    def is_saturn_a(self) -> bool:
        return self.map_name == "saturn_a"

    def is_saturn_b(self) -> bool:
        return self.map_name == "saturn_b"

    def has_completed(self) -> bool:
        return self.mission_duration != MISSING_INFO

    def to_data_frame(self) -> pd.DataFrame:
        header = [
            "Trial ID",
            "Trial Number",
            "Trial Start",
            "Trial End",
            "Trial Duration (sec)",
            "Mission Start",
            "Mission End",
            "Mission Duration (sec)",
            "Team Score",
            "Num Messages",
            "Num ASR Messages",
            "Num Dialog Messages",
            "Intervention Agents"
        ]

        data = [
            self.trial_id,
            self.trial_number,
            MISSING_INFO if self.trial_start is None else self.trial_start.isoformat(),
            MISSING_INFO if self.trial_end is None else self.trial_end.isoformat(),
            self.trial_duration,
            MISSING_INFO if self.mission_start is None else self.mission_start.isoformat(),
            MISSING_INFO if self.mission_end is None else self.mission_end.isoformat(),
            self.mission_duration,
            self.team_score,
            self.num_messages,
            self.num_asr_messages,
            self.num_dialog_messages,
            self.intervention_agents
        ]

        return pd.DataFrame([data], columns=header)


class USARSummary(TaskSummary):

    def __init__(self, mission_summaries_dict: Dict[str, USARMissionSummary]):
        super().__init__("USAR")

        self.mission_summaries_dict = mission_summaries_dict

    @classmethod
    def from_experiment_directory(cls, experiment_dir: str) -> USARSummary:
        metadata_filepaths = list(glob(f"{experiment_dir}/minecraft/*.metadata"))

        mission_summaries_dict = {
            "training": USARMissionSummary(),
            "main1": USARMissionSummary(),
            "main2": USARMissionSummary()
        }
        for metadata_filepath in metadata_filepaths:
            mission_summary = USARMissionSummary.from_metadata_file(metadata_filepath)

            # We replace the current mission the dictionary with the new one that has completed, or if the one in the
            # dictionary has not completed. This is because there will be at most one mission of each type completed,
            # but there can be many that were interrupted for some reason. We want to keep only the mission
            # that has been completed in the dictionary. In case there's none, we replace the default mission (set
            # in the creation of the dictionary), with any uncompleted real one.
            if mission_summary.is_training():
                if not mission_summaries_dict["training"].has_completed() or mission_summary.has_completed():
                    mission_summaries_dict["training"] = mission_summary
            elif mission_summary.is_saturn_a():
                mission_summaries_dict["main1"] = mission_summary
            elif mission_summary.is_saturn_b():
                mission_summaries_dict["main2"] = mission_summary

        return cls(mission_summaries_dict)

    def to_data_frame(self) -> pd.DataFrame:
        dfs = [
            self.mission_summaries_dict["training"].to_data_frame().add_suffix(f" ({self.task_name} - Training)"),
            self.mission_summaries_dict["main1"].to_data_frame().add_suffix(f" ({self.task_name} - Main 1)"),
            self.mission_summaries_dict["main2"].to_data_frame().add_suffix(f" ({self.task_name} - Main 2)")
        ]

        return pd.concat(dfs, axis=1)
