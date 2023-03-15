from __future__ import annotations

from glob import glob
import json
import pandas as pd

from summary.task_summary import TaskSummary
from common.constants import MISSING_INFO


class CompetitivePingPongSummary(TaskSummary):

    def __init__(self, participant_id_left: str, participant_id_right: str, score_left: str, score_right: str, team_id: int):
        super().__init__("Competitive Ping-Pong")
        self.participant_id_left = participant_id_left
        self.participant_id_right = participant_id_right
        self.score_left = score_left
        self.score_right = score_right
        self.team_id = team_id

    @classmethod
    def from_experiment_directory(cls, experiment_dir: str, team_id: int) -> CompetitivePingPongSummary:
        # We sort just in case there are multiple entries. This can happen if a game was interrupted, and we started
        # over. The timestamp appended after the team identifier in the filename help us to identify the latest file
        # recorded.
        game_files = sorted(list(glob(f"{experiment_dir}/baseline_tasks/ping_pong/competitive_{team_id - 1}_*.csv")))

        if len(game_files) == 0:
            # There was some error during the experiment and the file was not created. Data was not saved for the task.
            participant_id_left = MISSING_INFO
            participant_id_right = MISSING_INFO
            score_left = MISSING_INFO
            score_right = MISSING_INFO
        else:
            game_filepath = game_files[-1]
            game_df = pd.read_csv(game_filepath, delimiter=";")

            if len(game_df.columns) < 4:
                # Paulo Soares:
                # Old format where there's only 2 columns and the second is a json. This data is too old and it's
                # not used since we started the true pilots. So I will not worry about extract data from it.
                participant_id_left = MISSING_INFO
                participant_id_right = MISSING_INFO
                score_left = MISSING_INFO
                score_right = MISSING_INFO
            else:
                participant_id_left = game_df.columns[-4].split("_")[0]  # Column named <participant_id1>_y
                participant_id_right = game_df.columns[-3].split("_")[0]  # Column named <participant_id2>_x
                score_left = str(game_df.iloc[-1]["score_left"])
                score_right = str(game_df.iloc[-1]["score_right"])

        return cls(
            participant_id_left=participant_id_left,
            participant_id_right=participant_id_right,
            score_left=score_left,
            score_right=score_right,
            team_id=team_id
        )

    def to_data_frame(self) -> pd.DataFrame:
        header = [
            f"Participant ID Left",
            f"Participant ID Right",
            f"Score left",
            f"Score Right"
        ]
        # Add info about the team to the header
        header = [f"{h} ({self.task_name} - Team {self.team_id})" for h in header]

        data = [
            self.participant_id_left,
            self.participant_id_right,
            self.score_left,
            self.score_right
        ]

        return pd.DataFrame([data], columns=header)
