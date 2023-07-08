import os
import json
import pandas as pd
from termcolor import colored

def read_ping_pong_timestamps(
    block, input_path, output_path
):
    # Get experiment name from input path
    exp_name = os.path.basename(input_path)

    for i in range(0, len(block)):
        if block[i]["info"]["name"][0] == "PingPong_competitive_0":
            # Lion and Tiger compete against each other.
            print(
                colored("[Info] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
            )

            # print(block[i]['time_series'])
            PingPong_competitive_0 = block[i]["time_series"]
            PingPong_competitive_0_strings = [
                item[0] for item in PingPong_competitive_0
            ]

            # Parse JSON strings into dictionaries
            data = [json.loads(d) for d in PingPong_competitive_0_strings]

            # Convert list of dictionaries to DataFrame
            PingPong_competitive_0 = pd.DataFrame(data)
            PingPong_competitive_0["lsl_timestamp"] = block[i]["time_stamps"]
            PingPong_competitive_0_timestamp = int(
                PingPong_competitive_0["lsl_timestamp"].iloc[0]
            )

            full_path = os.path.join(
                output_path,
                exp_name,
                "baseline_tasks/ping_pong/competitive_0_"
                + str(PingPong_competitive_0_timestamp)
                + ".csv",
            )
            PingPong_competitive_0.to_csv(full_path, sep=";", encoding="utf-8")

            print(
                colored(
                    "[Status] Saving Ping Pong Competetive 0 to ",
                    "green",
                    attrs=["bold"],
                ),
                colored(full_path, "blue"),
            )

        if block[i]["info"]["name"][0] == "PingPong_competitive_1":
            # Leopard and experimentor compete with each other.
            print(
                colored("[Info] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
            )

            # print(block[i]['time_series'])
            PingPong_competitive_1 = block[i]["time_series"]
            PingPong_competitive_1_strings = [
                item[0] for item in PingPong_competitive_1
            ]

            # Parse JSON strings into dictionaries
            data = [json.loads(d) for d in PingPong_competitive_1_strings]

            # Convert list of dictionaries to DataFrame
            PingPong_competitive_1 = pd.DataFrame(data)
            PingPong_competitive_1["lsl_timestamp"] = block[i]["time_stamps"]
            PingPong_competitive_1_timestamp = int(
                PingPong_competitive_1["lsl_timestamp"].iloc[0]
            )

            full_path = os.path.join(
                output_path,
                exp_name,
                "baseline_tasks/ping_pong/competitive_1_"
                + str(PingPong_competitive_1_timestamp)
                + ".csv",
            )
            PingPong_competitive_1.to_csv(full_path, sep=";", encoding="utf-8")
            print(
                colored(
                    "[Status] Saving Ping Pong Competetive 1 to ",
                    "green",
                    attrs=["bold"],
                ),
                colored(full_path, "blue"),
            )

        if block[i]["info"]["name"][0] == "PingPong_cooperative_0":
            # Lion, Tiger and Leopard cooperate and compete againsts an AI.
            print(
                colored("[Info] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
            )

            # print(block_1[i]['time_series'])
            PingPong_cooperative_0 = block[i]["time_series"]
            PingPong_cooperative_0_strings = [
                item[0] for item in PingPong_cooperative_0
            ]

            # Parse JSON strings into dictionaries
            data = [json.loads(d) for d in PingPong_cooperative_0_strings]

            # Convert list of dictionaries to DataFrame
            PingPong_cooperative_0 = pd.DataFrame(data)
            PingPong_cooperative_0["lsl_timestamp"] = block[i]["time_stamps"]
            PingPong_cooperative_0_timestamp = int(
                PingPong_cooperative_0["lsl_timestamp"].iloc[0]
            )

            full_path = os.path.join(
                output_path,
                exp_name,
                "baseline_tasks/ping_pong/cooperative_0_"
                + str(PingPong_cooperative_0_timestamp)
                + ".csv",
            )
            PingPong_cooperative_0.to_csv(full_path, sep=";", encoding="utf-8")
            print(
                colored(
                    "[Status] Saving Ping Pong Coorperative 0 to ",
                    "green",
                    attrs=["bold"],
                ),
                colored(full_path, "blue"),
            )

