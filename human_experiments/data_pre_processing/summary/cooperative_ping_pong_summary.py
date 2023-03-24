from __future__ import annotations

from glob import glob
import pandas as pd

from summary.task_summary import TaskSummary
from common.constants import MISSING_INFO


class CooperativePingPongSummary(TaskSummary):

    def __init__(self, team_score: str, ai_score: str):
        super().__init__("Cooperative Ping-Pong")
        self.team_score = team_score
        self.ai_score = ai_score

    @classmethod
    def from_experiment_directory(cls, experiment_dir: str) -> CooperativePingPongSummary:
        # We sort just in case there are multiple entries. This can happen if a game was interrupted, and we started
        # over. The timestamp appended after the team identifier in the filename help us to identify the latest file
        # recorded.
        game_files = sorted(list(glob(f"{experiment_dir}/baseline_tasks/ping_pong/cooperative_0_*.csv")))

        if len(game_files) == 0:
            # There was some error during the experiment and the file was not created. Data was not saved for the task.
            return CooperativePingPongSummary.empty_summary()
        else:
            try:
                game_filepath = game_files[-1]
                game_df = pd.read_csv(game_filepath, delimiter=";")

                if "score_left" not in game_df.columns:
                    # Paulo Soares:
                    # Old format where some cells contain json data. This data is too old and it's not used since we
                    # started the true pilots. So I will not worry about extract data from it.
                    return CooperativePingPongSummary.empty_summary()
                else:
                    return cls(
                        team_score=str(game_df.iloc[-1]["score_left"]),
                        ai_score=str(game_df.iloc[-1]["score_right"])
                    )

            except pd.errors.EmptyDataError:
                return CooperativePingPongSummary.empty_summary()

    @classmethod
    def empty_summary(cls) -> CooperativePingPongSummary:
        return cls(
            team_score=MISSING_INFO,
            ai_score=MISSING_INFO
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

        return pd.DataFrame([data], columns=header)
