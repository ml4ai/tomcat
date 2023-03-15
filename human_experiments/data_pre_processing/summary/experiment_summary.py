from __future__ import annotations
from typing import List

import pandas as pd
import os

from human_experiments.data_pre_processing.summary.competitive_ping_pong_summary import CompetitivePingPongSummary
from human_experiments.data_pre_processing.summary.cooperative_ping_pong_summary import CooperativePingPongSummary
from human_experiments.data_pre_processing.summary.redcap_summary import TeamREDCapSummary, ParticipantREDCapSummary
from human_experiments.data_pre_processing.summary.task_summary import TaskSummary
from human_experiments.data_pre_processing.summary.usar_summary import USARSummary


class ExperimentSummary:

    def __init__(self, experiment_id: str, team_summary: TeamREDCapSummary, lion_summary: ParticipantREDCapSummary,
                 tiger_summary: ParticipantREDCapSummary, leopard_summary: ParticipantREDCapSummary,
                 task_summaries: List[TaskSummary]):
        self.experiment_id = experiment_id
        self.team_summary = team_summary
        self.lion_summary = lion_summary
        self.tiger_summary = tiger_summary
        self.leopard_summary = leopard_summary
        self.task_summaries = task_summaries

    @classmethod
    def from_experiment_directory(cls, experiment_dir: str) -> ExperimentSummary:
        task_summaries = [
            CompetitivePingPongSummary.from_experiment_directory(experiment_dir, team_id=1),
            CompetitivePingPongSummary.from_experiment_directory(experiment_dir, team_id=2),
            CooperativePingPongSummary.from_experiment_directory(experiment_dir),
            USARSummary.from_experiment_directory(experiment_dir)
        ]

        return cls(
            experiment_id=os.path.basename(experiment_dir),
            team_summary=TeamREDCapSummary.from_experiment_directory(experiment_dir),
            lion_summary=ParticipantREDCapSummary.from_experiment_directory(experiment_dir, "lion"),
            tiger_summary=ParticipantREDCapSummary.from_experiment_directory(experiment_dir, "tiger"),
            leopard_summary=ParticipantREDCapSummary.from_experiment_directory(experiment_dir, "leopard"),
            task_summaries=task_summaries
        )

    def to_data_frame(self) -> pd.DataFrame:
        header = [
            "Experiment ID",
        ]

        data = [
            self.experiment_id
        ]

        experiment_df = pd.DataFrame(data, columns=header)

        dfs = [
            experiment_df,
            self.team_summary.to_data_frame(),
            self.lion_summary.to_data_frame(),
            self.tiger_summary.to_data_frame(),
            self.leopard_summary.to_data_frame()
        ]
        dfs.extend([task_summary.to_data_frame() for task_summary in self.task_summaries])

        return pd.merge(dfs, axis=1)
