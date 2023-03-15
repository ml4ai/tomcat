from __future__ import annotations

from glob import glob
import pandas as pd

from human_experiments.data_pre_processing.summary.task_summary import TaskSummary


class CooperativePingPongSummary(TaskSummary):

    def __init__(self, team_score: int, ai_score: int):
        super().__init__("Cooperative Ping-Pong")
        self.team_score = team_score
        self.ai_score = ai_score

    @classmethod
    def from_experiment_directory(cls, experiment_dir: str) -> CooperativePingPongSummary:
        # We sort just in case there are multiple entries. This can happen if a game was interrupted, and we started
        # over. The timestamp appended after the team identifier in the filename help us to identify the latest file
        # recorded.
        game_filepath = sorted(list(glob(f"{experiment_dir}/baseline_tasks/competitive_0_*.csv")))[-1]
        game_df = pd.read_csv(game_filepath, delimiter=";")

        team_score = int(game_df.iloc[-1]["score_left"])
        ai_score = int(game_df.iloc[-1]["score_right"])

        return cls(
            team_score=team_score,
            ai_score=ai_score
        )

    def to_data_frame(self) -> pd.DataFrame:
        header = [
            f"Team Score",
            f"AI Score"
        ]
        # Add info about the team to the header
        header = [f"{h} ({self.task_name})" for h in header]

        data = [
            self.team_score,
            self.ai_score
        ]

        return pd.DataFrame(data, columns=header)
