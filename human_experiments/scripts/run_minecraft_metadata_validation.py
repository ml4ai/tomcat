from itertools import count
import sys  # We use sys.exit() so that all exceptions can properly propogate up and cause the interpreter to exit.
import os
import json
import argparse
import datetime
from termcolor import colored


def check_time_difference(mission_start, mission_end):
    """
    The timestamps for the start and end of the trial are published in
    ISO-8601 format, which has to be converted into datetime format
    '%Y-%m-%d %H:%M:%S.%f' from which the difference is calculated to
    find out the time taken by a mission.
    """
    mission_start = (
        mission_start.split("T")[0]
        + " "
        + mission_start.split("T")[1].split("Z")[0]
    )
    mission_end = (
        mission_end.split("T")[0] + " " + mission_end.split("T")[1].split("Z")[0]
    )

    print(
        colored("\n[Status] Mission started at time:", "red", attrs=["bold"]),
        mission_start,
    )
    print(
        colored("\n[Status] Mission ended at time:", "red", attrs=["bold"]),
        mission_end,
    )

    mission_start = datetime.datetime.strptime(
        str(mission_start), "%Y-%m-%d %H:%M:%S.%f"
    )
    mission_start = mission_start.timestamp()
    mission_end = datetime.datetime.strptime(
        str(mission_end), "%Y-%m-%d %H:%M:%S.%f"
    )
    mission_end = mission_end.timestamp()
    delta = int(float(mission_end) - float(mission_start))
    min, sec = divmod(delta, 60)

    if min > 0:
        print(
            colored("\n[Status] Trial lasted for: ", "red", attrs=["bold"]),
            min,
            "Minutes",
            sec,
            "Seconds",
        )
    else:
        print(
            colored(
                "[Error] Timing for mission is off by a large margin", "red"
            ),
            "\N{cross mark}",
        )


def read_subject_id(TrialMessages):

    print(colored("\n[Status] Subject info:", "red", attrs=["bold"]))
    try:
        # Display subject IDs
        for idx, sub in enumerate(
            TrialMessages[0]["data"]["metadata"]["trial"]["subjects"]
        ):
            print(colored("\t Subect ID", "magenta"), idx, ":", sub)
        # Display trial name
        print(
            colored("\n[Status] Mission name:", "red", attrs=["bold"]),
            TrialMessages[0]["data"]["metadata"]["trial"]["name"],
        )
    except:
        # Display subject IDs
        for idx, sub in enumerate(
            TrialMessages[0]["data"]["subjects"]
        ):
            print(colored("\t Subect ID", "magenta"), idx, ":", sub)        
        # Display trial name
        print(
            colored("\n[Status] Mission name:", "red", attrs=["bold"]),
            TrialMessages[0]['data']['experiment_mission'],
        )


def read_metadata_as_json(path):
    """
    This function reads in a .metadata file, and performs some basic checks,
    prints out the subject IDs, and checks the time difference between the
    mission start and end times.

    For the trial start and stop messages, it checks that:
    - .header.message_type = "event"
    - .msg.sub_type = "Event:MissionState"

    It then reads the mission start and stop timestamps from the .msg.timestamp
    field.
    """
    TrialMessages = []
    count = 0
    with open(path, "r") as f:
        condition = []
        for line in f:
            count += 1
            try:
                json_message = json.loads(line)
                TrialMessages.append(json_message)
                if (
                    json_message["header"]["message_type"] == "event"
                    and json_message["msg"]["sub_type"] == "Event:MissionState"
                ):
                    if json_message["data"]["mission_state"] == "Start":
                        mission_start = json_message["msg"]["timestamp"]
                        condition.append('Start')
                    if json_message["data"]["mission_state"] == "Stop":
                        mission_end = json_message["msg"]["timestamp"]
                        condition.append('Stop')
                        
                if len(condition) == 2:
                    continue
                else: 
                    '''
                    There can be issue where the experimentor ends the mission
                    abruptly, then mission_state wouldn't have stop message. 
                    Switch to trial message instead and see when the trial 
                    ended and use that as mission_end. 
                    '''
                    if (
                        json_message["header"]["message_type"] == "trial"
                        ):
                        if json_message["msg"]["sub_type"] == "stop":
                            print(
                            colored("[Warning] Mission end not found. Using trial end as mission end!", "yellow")
                            )
                            mission_end = json_message["msg"]["timestamp"]
                        
            except:
                print(
                    colored("[Error] Cannot read JSON line", "red"),
                    "\N{cross mark}",
                )
    read_subject_id(TrialMessages)
    check_time_difference(mission_start, mission_end)


def checkfile(rootdir):
    """
    This function checks if the .metadata file is present under the given path or not. It also checks
    if the .metadata file is empty or not.
    """
    dir = os.listdir(rootdir)

    if len(dir) == 0:
        # sometime .DS_Store might be there so the length would be 1
        print(
            colored(
                "\n[Error] Metadata file is missing", "red", attrs=["bold"]
            ),
            "\N{cross mark}",
        )
    else:
        for x in os.listdir(rootdir):
            if x.endswith(".metadata"):
                # check if .metadata file exists or not
                print(
                    colored(
                        "\n[Status] Metadata File:", "red", attrs=["bold"]
                    ),
                    colored(os.path.join(rootdir, x), "green"),
                    "\N{check mark}",
                )

                if os.stat(os.path.join(rootdir, x)).st_size == 0:
                    # check if .metadata file is empty or not
                    print(
                        colored(
                            "\n[Error] Metadata file is empty",
                            "red",
                            attrs=["bold"],
                        ),
                        "\N{cross mark}",
                    )
                else:
                    read_metadata_as_json(os.path.join(rootdir, x))
            elif x.endswith(".DS_Store"):
                # The .DS_Store file might be there sometimes
                continue
            else:
                print(
                    colored(
                        "\n[Error] Metadata file is missing",
                        "red",
                        attrs=["bold"],
                    ),
                    "\N{cross mark}",
                )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Minecraft data validation script"
    )
    parser.add_argument(
        "--p",
        required=True,
        help="Path to the folder with the Minecraft data",
    )
    arg = parser.parse_args()
    rootdir = arg.p
    print(
        colored("[Status] Root Directory:", "red", attrs=["bold"]),
        colored(rootdir, "blue"),
    )
    sys.exit(checkfile(rootdir))
