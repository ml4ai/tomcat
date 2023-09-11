import pandas as pd


class TaskSummary:

    def __init__(self, task_name: str):
        self.task_name = task_name

    def to_data_frame(self) -> pd.DataFrame:
        # This method must be implemented by a child class
        raise NotImplemented
