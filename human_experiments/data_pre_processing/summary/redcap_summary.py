from __future__ import annotations

import pandas as pd
import os.path

from common.constants import MISSING_INFO


class REDCapSummary:

    def to_data_frame(self) -> pd.DataFrame:
        # This method must be implemented by the child classes
        raise NotImplemented


class TeamREDCapSummary(REDCapSummary):

    def __init__(self, experiment_date: str, team_number: str, participants_issues_details: str,
                 equipment_issues_details: str, additional_notes: str):
        super().__init__()
        self.experiment_date = experiment_date
        self.team_number = team_number
        self.participants_issues_details = participants_issues_details
        self.equipment_issues_details = equipment_issues_details
        self.additional_notes = additional_notes

    @classmethod
    def from_experiment_directory(cls, experiment_dir: str) -> TeamREDCapSummary:
        redcap_filepath = f"{experiment_dir}/REDCap_data/Team_Data.csv"

        if os.path.exists(redcap_filepath):
            redcap_df = pd.read_csv(redcap_filepath, delimiter=",")

            return cls(
                experiment_date=redcap_df.iloc[0]["testing_session_date"],
                team_number=redcap_df.iloc[0]["team_id"],
                participants_issues_details=redcap_df.iloc[0]["participants_issues_details"],
                equipment_issues_details=redcap_df.iloc[0]["equipment_issues_details"],
                additional_notes=redcap_df.iloc[0]["additional_notes"]
            )
        else:
            return cls(
                experiment_date=MISSING_INFO,
                team_number=MISSING_INFO,
                participants_issues_details=MISSING_INFO,
                equipment_issues_details=MISSING_INFO,
                additional_notes=MISSING_INFO
            )

    def to_data_frame(self) -> pd.DataFrame:
        headers = [
            "Experiment Date",
            "Team Number",
            "Participant Issues",
            "Equipment Issues",
            "Additional Notes"
        ]

        data = [
            self.experiment_date,
            self.team_number,
            self.participants_issues_details,
            self.equipment_issues_details,
            self.additional_notes
        ]

        return pd.DataFrame([data], columns=headers)


class ParticipantREDCapSummary(REDCapSummary):

    def __init__(self, participant_id: str):
        super().__init__()
        self.participant_id = participant_id

    @classmethod
    def from_experiment_directory(cls, experiment_dir: str, machine_name: str) -> ParticipantREDCapSummary:
        redcap_filenames = []
        redcap_dir = f"{experiment_dir}/{machine_name}/REDCap_data/"
        if os.path.exists(redcap_dir):
            redcap_filenames = [filename for filename in os.listdir(redcap_dir) if
                                "post_game_survey" in filename.lower()]

        if len(redcap_filenames) == 0:
            return cls(
                participant_id=MISSING_INFO
            )
        else:
            redcap_filepath = f"{redcap_dir}/{redcap_filenames[0]}"
            redcap_df = pd.read_csv(redcap_filepath, delimiter=",")

            return cls(
                participant_id=redcap_df.iloc[0]["subject_id"]
            )

    def to_data_frame(self) -> pd.DataFrame:
        headers = [
            "Participant ID"
        ]

        data = [
            self.participant_id
        ]

        return pd.DataFrame([data], columns=headers)
