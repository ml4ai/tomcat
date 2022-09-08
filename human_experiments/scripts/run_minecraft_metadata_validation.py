import sys  # We use sys.exit() so that all exceptions can properly propogate up and cause the interpreter to exit.
import os
import json
import argparse
import datetime
from termcolor import colored


def check_time_difference(trial_start, trial_end):
    """
    The timestamps for the start and end of the trial are published in
    ISO-8601 format, which has to be converted into datetime format
    '%Y-%m-%d %H:%M:%S.%f' from which the difference is calculated to
    find out the time taken by a mission.
    """
    trial_start = (
        trial_start.split("T")[0]
        + " "
        + trial_start.split("T")[1].split("Z")[0]
    )
    trial_end = (
        trial_end.split("T")[0] + " " + trial_end.split("T")[1].split("Z")[0]
    )

    print(
        colored("\n[Status] Mission started at time:", "red", attrs=["bold"]),
        trial_start,
    )
    print(
        colored("\n[Status] Mission ended at time:", "red", attrs=["bold"]),
        trial_end,
    )

    trial_start = datetime.datetime.strptime(
        str(trial_start), "%Y-%m-%d %H:%M:%S.%f"
    )
    trial_start = trial_start.timestamp()
    trial_end = datetime.datetime.strptime(
        str(trial_end), "%Y-%m-%d %H:%M:%S.%f"
    )
    trial_end = trial_end.timestamp()
    delta = int(float(trial_end) - float(trial_start))
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
    # Display subject IDs
    print(colored("\n[Status] Subject info:", "red", attrs=["bold"]))
    for idx, sub in enumerate(
        TrialMessages[0]["data"]["metadata"]["trial"]["subjects"]
    ):
        print(colored("\t Subect ID", "magenta"), idx, ":", sub)

    # Display trial name
    print(
        colored("\n[Status] Mission name:", "red", attrs=["bold"]),
        TrialMessages[0]["data"]["metadata"]["trial"]["name"],
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
    with open(path, "r") as f:
        for line in f:
            try:
                json_message = json.loads(line)
                TrialMessages.append(json_message)
                if (
                    json_message["header"]["message_type"] == "event"
                    and json_message["msg"]["sub_type"] == "Event:MissionState"
                ):
                    if json_message["data"]["mission_state"] == "Start":
                        mission_start = json_message["msg"]["timestamp"]
                    else:
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
        help="Path to the folder with the baseline task data",
    )
    arg = parser.parse_args()
    rootdir = arg.p
    print(
        colored("[Status] Root Directory:", "red", attrs=["bold"]),
        colored(rootdir, "blue"),
    )
    sys.exit(checkfile(rootdir))
