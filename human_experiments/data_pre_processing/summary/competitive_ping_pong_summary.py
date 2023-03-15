from __future__ import annotations

from glob import glob
import pandas as pd

from human_experiments.data_pre_processing.summary.task_summary import TaskSummary


class CompetitivePingPongSummary(TaskSummary):

    def __init__(self, participant_id_left: str, participant_id_right: str, score_left: int, score_right: int, team_id: int):
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
        game_filepath = sorted(list(glob(f"{experiment_dir}/baseline_tasks/competitive_{team_id - 1}_*.csv")))[-1]
        game_df = pd.read_csv(game_filepath, delimiter=";")

        participant_id_left = game_df.columns[-4].split("_")[0]  # Column named <participant_id1>_y
        participant_id_right = game_df.columns[-3].split("_")[0]  # Column named <participant_id2>_x
        score_left = int(game_df.iloc[-1]["score_left"])
        score_right = int(game_df.iloc[-1]["score_right"])

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

        return pd.DataFrame(data, columns=header)
