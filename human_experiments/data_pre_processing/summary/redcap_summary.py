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
        # Name of the directory containing REDCap data. We look for it in a list because the case changed over time.
        redcap_dirs = [dir_name for dir_name in os.listdir(f"{experiment_dir}") if
                       "redcap" in dir_name.lower()]

        if len(redcap_dirs) > 0:
            try:
                # We need this logic because the folder and the file name changes case over the time.
                redcap_dir = f"{experiment_dir}/{redcap_dirs[0]}"
                redcap_filenames = [filename for filename in os.listdir(redcap_dir) if "team_data" in filename.lower()]

                if len(redcap_filenames) == 0:
                    return TeamREDCapSummary.empty_summary()
                else:
                    redcap_filepath = f"{redcap_dir}/{redcap_filenames[0]}"
                    redcap_df = pd.read_csv(redcap_filepath, delimiter=",")

                    return cls(
                        experiment_date=redcap_df.iloc[0]["testing_session_date"],
                        team_number=redcap_df.iloc[0]["team_id"],
                        participants_issues_details=redcap_df.iloc[0]["participants_issues_details"],
                        equipment_issues_details=redcap_df.iloc[0]["equipment_issues_details"],
                        additional_notes=redcap_df.iloc[0]["additional_notes"]
                    )
            except pd.errors.EmptyDataError:
                return TeamREDCapSummary.empty_summary()
        else:
            return TeamREDCapSummary.empty_summary()

    @classmethod
    def empty_summary(cls) -> TeamREDCapSummary:
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
        # Name of the directory containing REDCap data. We look for it in a list because the case changed over time.
        redcap_dirs = [dir_name for dir_name in os.listdir(f"{experiment_dir}/{machine_name}") if
                       "redcap" in dir_name.lower()]

        if len(redcap_dirs) > 0:
            redcap_dir = f"{experiment_dir}/{machine_name}/{redcap_dirs[0]}"
            redcap_filenames = [filename for filename in os.listdir(redcap_dir) if
                                "post_game_survey" in filename.lower()]

            if len(redcap_filenames) == 0:
                return ParticipantREDCapSummary.empty_summary()
            else:
                try:
                    redcap_filepath = f"{redcap_dir}/{redcap_filenames[0]}"
                    redcap_df = pd.read_csv(redcap_filepath, delimiter=",")

                    return cls(
                        # If I don't explicitly transform to string, it will retrieve the participant id as a number, and it
                        # will erase leading zeros.
                        participant_id=str(redcap_df.iloc[0]["subject_id"])
                    )

                except pd.errors.EmptyDataError:
                    return ParticipantREDCapSummary.empty_summary()
        else:
            return ParticipantREDCapSummary.empty_summary()

    @classmethod
    def empty_summary(cls) -> ParticipantREDCapSummary:
        return cls(
            participant_id=MISSING_INFO
        )

    def to_data_frame(self) -> pd.DataFrame:
        headers = [
            "Participant ID"
        ]

        data = [
            self.participant_id
        ]

        return pd.DataFrame([data], columns=headers)
