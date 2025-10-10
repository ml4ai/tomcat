import os
import json
import pandas as pd
from termcolor import colored


def read_finger_tapping_time(block, rest_state_marker, input_path, output_path):
    idx = len(rest_state_marker)
    markers = rest_state_marker
    for i in range(0, len(block)):
        if block[i]["info"]["name"][0] == "FingerTapping":
            print(
                colored("[Info] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
            )

            finger_tapping = block[i]["time_series"]
            finger_tapping_strings = [item[0] for item in finger_tapping]

            # Parse JSON strings into dictionaries
            data = [json.loads(d) for d in finger_tapping_strings]

            # Convert list of dictionaries to DataFrame
            df = pd.DataFrame(data)
            df["lsl_timestamp"] = block[i]["time_stamps"]
            timestamp = int(df["lsl_timestamp"].iloc[0])

            markers[idx] = {
                "state": "finger_tapping",
                "participant": None,
                "start_time": df["lsl_timestamp"].iloc[0],
                "end_time": df["lsl_timestamp"].iloc[-1],
            }

            # Get experiment name from input path
            exp_name = os.path.basename(input_path)

            # Create full file path
            full_path = os.path.join(
                output_path,
                exp_name,
                "baseline_tasks/finger_tapping/" + str(timestamp) + ".csv",
            )
            df.to_csv(full_path, sep=";")
            print(
                colored(
                    "[Status] Saving Finger Tapping task to ", "green", attrs=["bold"]
                ),
                colored(full_path, "blue"),
            )
            idx += 1

    print(markers)
    return markers
