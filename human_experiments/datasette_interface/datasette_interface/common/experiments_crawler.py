import os
from typing import Callable

from tqdm import tqdm

from datasette_interface.common.config import settings
from datasette_interface.common.utils import (
    is_directory_with_unified_xdf_files,
    should_ignore_directory,
)


class ExperimentsCrawler:
    """
    This class represents a crawler that navigates through the experiment folders under a specific
    directory and invokes callbacks for treatment of the data in the experiment.
    """

    def __init__(self, callback: Callable):
        """
        Creates a crawler.

        :param callback: function to be called at each experiment directory visit. This function
            must have the following signature: def callback(experiment_dir, has_unified_xdf) where
            experiment_dir will receive the directory where experiment files are located and
            has_unified_xdf will contain a boolean indicating whether the experiment has unified
            xdf files or not.
        """
        self.callback = callback

    def crawl(self):
        """
        Loops through the experiments invoking callbacks for data processing.
        """
        experiment_ids = []
        for experiment_id in os.listdir(settings.experiment_root_dir):
            experiment_dir = f"{settings.experiment_root_dir}/{experiment_id}"
            if not os.path.isdir(experiment_dir):
                continue

            if experiment_id[:3] != "exp":
                continue

            experiment_ids.append(experiment_id)

        experiment_ids.sort()
        for experiment_id in tqdm(experiment_ids):
            experiment_dir = f"{settings.experiment_root_dir}/{experiment_id}"
            if not os.path.isdir(experiment_dir):
                continue

            if experiment_id[:3] != "exp":
                continue

            if not should_ignore_directory(experiment_id):
                self.callback(
                    experiment_dir,
                    is_directory_with_unified_xdf_files(experiment_id),
                )
