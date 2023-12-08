import os

from data_pre_processing.common.constants import DEFAULT_EXPERIMENTS_ROOT_DIR
import logging
from typing import Callable
from tqdm import tqdm

info = logging.getLogger().info


class ExperimentsCrawler:
    """
    This class represents a crawler that navigates through the experiment folders under a specific
    directory and invokes callbacks for treatment of the data in the experiment.
    """

    def __init__(self,
                 callback: Callable,
                 experiments_root_dir: str = DEFAULT_EXPERIMENTS_ROOT_DIR):
        """
        Creates a crawler.

        :param callback: function to be called at each experiment directory visit. This function
            must have the following signature: def callback(experiment_dir, has_unified_xdf) where
            experiment_dir will receive the directory where experiment files are located and
            has_unified_xdf will contain a boolean indicating whether the experiment has unified
            xdf files or not.
        :param experiments_root_dir: folder under where all the experiments are located.
        """
        self.callback = callback
        self.experiments_root_dir = experiments_root_dir

    def crawl(self):
        """
        Loops through the experiments invoking callbacks for data processing.
        """
        experiment_ids = []
        for experiment_id in os.listdir(self.experiments_root_dir):
            experiment_dir = f"{self.experiments_root_dir}/{experiment_id}"
            if not os.path.isdir(experiment_dir):
                continue

            if experiment_id[:3] != "exp":
                continue

            experiment_ids.append(experiment_id)

        experiment_ids.sort()
        for experiment_id in tqdm(experiment_ids):
            experiment_dir = f"{self.experiments_root_dir}/{experiment_id}"
            if not os.path.isdir(experiment_dir):
                continue

            if experiment_id[:3] != "exp":
                continue

            if not ExperimentsCrawler._should_ignore_experiment(experiment_id):
                self.callback(
                    experiment_dir,
                    ExperimentsCrawler._has_experiment_have_unified_xdf_files(experiment_id)
                )

    @staticmethod
    def _should_ignore_experiment(experiment_id: str) -> bool:
        """
        Checks whether the directory should be ignored and why. It logs info why it should be
        ignored.

        :param experiment_id: id of the experiment.
        :return: true if it should be ignored.
        """
        year, month, day, hour = [int(x) for x in experiment_id.split("_")[1:]]

        if year == 2022 and ((month < 9) or (month == 9 and day < 30)):
            info(
                f"Ignoring {experiment_id} since our first pilot with real "
                "participants was on 9/30/2022."
            )
            return True
        elif experiment_id == "exp_2023_04_20_14":
            info(
                f"Ignoring {experiment_id}. Since only one participant showed up, the session was "
                f"cancelled."
            )
            return True

        elif experiment_id == "exp_2023_02_20_13":
            info(
                f"Ignoring {experiment_id}, since it is a duplicate of the "
                "exp_2023_02_20_01 directory."
            )
            return True

        elif experiment_id in {"exp_2022_12_05_15", "exp_2023_04_26_10"}:
            info(
                f"Ignoring {experiment_id}, since it was cancelled (no participants showed up)."
            )
            return True
        else:
            return False

    @staticmethod
    def _has_experiment_have_unified_xdf_files(experiment_id: str) -> bool:
        """
        Checks whether the experiment is part happened after we redesigned the experimental
        procedure to have all data handled by LSL and saved in a single .xdf file.

        :param experiment_id: id of the experiment.
        :return: true if experiment after experimental procedure redesigning.
        """
        year, month, day, hour = [int(x) for x in experiment_id.split("_")[1:]]
        return (year, month) >= (2023, 4)
