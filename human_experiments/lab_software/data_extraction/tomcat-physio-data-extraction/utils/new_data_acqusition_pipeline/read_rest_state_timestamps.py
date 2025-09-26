import os
from termcolor import colored
import json
import pandas as pd


def read_rest_state_timestamps(block, input_path, output_path):
    markers = {}
    idx = 0
    for i in range(0, len(block)):
        if block[i]["info"]["name"][0] == "RestState":
            print(
                colored("[Info] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
            )

            # Get experiment name from input path
            exp_name = os.path.basename(input_path)

            rest_state_df = block[i]["time_series"]
            rest_state_df_strings = [item[0] for item in rest_state_df]
            data = [json.loads(d) for d in rest_state_df_strings]
            df = pd.DataFrame(data)
            df["lsl_timestamp"] = block[i]["time_stamps"]
            rest_state_timestamp = int(df["lsl_timestamp"].iloc[0])

            full_path = os.path.join(
                output_path,
                exp_name,
                "baseline_tasks/rest_state/" + str(rest_state_timestamp) + ".csv",
            )
            df.to_csv(full_path, sep=";", encoding="utf-8")
            print(
                colored("[Status] Saving Rest state to", "green", attrs=["bold"]),
                colored(full_path, "blue"),
            )

