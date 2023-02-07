import os
import json
import datetime
from termcolor import colored
from pathlib import Path

training_counter = 0
saturn_A_counter = 0
saturn_B_counter = 0


def check_asist_folder(rootdir, logging):
    """
    asist is usually under exp_*/testbed_logs/
    """
    if os.path.exists(rootdir + "testbed_logs"):
        print(
            colored("\n [Status] Asist folder exists:", "blue", attrs=["bold"]),
            colored(rootdir + "testbed_logs", "cyan"),
        )
        size = sum(p.stat().st_size for p in Path(rootdir + "testbed_logs").rglob("*"))
        if size > 1.5 * 1e9:
            print(
                colored("\n [Status] Asist folder is of size:", "blue", attrs=["bold"]),
                colored(size / (1024 * 1024 * 1024), "cyan"),
                colored("GB", "cyan"),
            )
        else:
            print(
                colored(
                    "\n [Status] Asist folder has an unexpected size. Please check!",
                    "red",
                    attrs=["bold"],
                ),
                colored(size / (1024 * 1024 * 1024), "cyan"),
                colored("GB", "cyan"),
            )
            logging.info(
                "\tASIST_folder\tUnexpected_size\t{}".format(
                    size / (1024 * 1024 * 1024)
                )
            )

    elif os.path.exists(rootdir):
        for _, dir, files in os.walk(rootdir):
            """
            check for the asist_logs_*.tar.gz file
            """
            if "asist_logs" in str(files)[1:-1]:
                print(
                    colored(
                        "\n [Warning] Asist tar file exists:", "yellow", attrs=["bold"]
                    ),
                    colored(rootdir + "/" + str(files)[1:-1], "cyan"),
                    colored("Please extract to testbed_logs", "yellow", attrs=["bold"]),
                )
                logging.info(
                    "\tASIST_folder\tExtract_asist_logs_tar_file_to_testbed_logs\t{}".format(
                        rootdir + "/" + rootdir.split("asist_logs", 1)[0]
                    )
                )

    else:
        print(
            colored("\n[Error] Asist folder does not exist", "red", attrs=["bold"]),
            "\N{cross mark}",
        )
        logging.info("\tASIST_folder\tDoesnot_exist\t")


def check_vocalics(rootdir, logging):
    """
    Vocalics is usually under exp_*/testbed_logs/asist_logs_*/speech_analyzer_agent
    """
    if os.path.exists(rootdir + "testbed_logs"):
        for root, dirs, files in os.walk(rootdir + "testbed_logs"):
            for name in files:
                try:
                    if name.endswith((".sql")):
                        if os.stat(root + "/" + name).st_size != 0:
                            print(
                                colored(
                                    "\n [Status] Voclic features found:",
                                    "blue",
                                    attrs=["bold"],
                                ),
                                colored(name, "cyan"),
                            )
                except:
                    print(
                        colored(
                            "\n [Error] Voclic features empty or not present",
                            "red",
                            attrs=["bold"],
                        ),
                        "\N{cross mark}",
                    )
                    logging.info(
                        "\tVocalics\tsql_file_not_found\t{}".format(root + "/" + name)
                    )
    else:
        print(
            colored(
                "\n[Error]Vocalics not found as Asist folder does not exist",
                "red",
                attrs=["bold"],
            ),
            "\N{cross mark}",
        )
        logging.info("\tVocalics\tDoesnot_exist_as_ASIST_folder_doesnt_exist\t")


def check_audio(rootdir, logging, x):
    """
    Audio file will be under exp_*/lion/audio, exp_*/tiger/audio, exp_*/leopard/audio
    """
    for _, _, files in os.walk(rootdir + x):
        for name in files:
            try:
                if name.endswith((".wav")):
                    if os.stat(rootdir + x + "/audio/" + name).st_size != 0:
                        print(
                            colored(
                                "\n [Status] Audio file for", "blue", attrs=["bold"]
                            ),
                            colored(x, "green", attrs=["bold"]),
                            colored("found", "blue", attrs=["bold"]),
                            colored(name, "cyan"),
                        )
                    else:
                        print(
                            colored(
                                "[Error] Audio file is empty", "red", attrs=["bold"]
                            ),
                            colored(x, "cyan"),
                            "\N{cross mark}",
                        )
                        logging.info(
                            "\tMinecraft_Audio\tAudio_file_empty\t{}".format(
                                rootdir + x + "/audio/" + name
                            )
                        )
            except:
                print(
                    colored("[Error] Audio file doesn't exist", attrs=["bold"]),
                    colored(x, "cyan"),
                )
                logging.info(
                    "\tMinecraft_Audio\tAudio_file_doesnot_exist\t{}".format(
                        rootdir + x + "/audio/" + name
                    )
                )


def check_time_difference(mission_start, mission_end, logging, path):
    """
    The timestamps for the start and end of the trial are published in
    ISO-8601 format, which has to be converted into datetime format
    '%Y-%m-%d %H:%M:%S.%f' from which the difference is calculated to
    find out the time taken by a mission.
    """
    mission_start = (
        mission_start.split("T")[0] + " " + mission_start.split("T")[1].split("Z")[0]
    )
    mission_end = (
        mission_end.split("T")[0] + " " + mission_end.split("T")[1].split("Z")[0]
    )

    print(
        colored("\n[Info] Mission started at time:", "blue", attrs=["bold"]),
        mission_start,
    )
    print(
        colored("\n[Info] Mission ended at time:", "blue", attrs=["bold"]),
        mission_end,
    )

    mission_start = datetime.datetime.strptime(
        str(mission_start), "%Y-%m-%d %H:%M:%S.%f"
    )
    mission_start = mission_start.timestamp()
    mission_end = datetime.datetime.strptime(str(mission_end), "%Y-%m-%d %H:%M:%S.%f")
    mission_end = mission_end.timestamp()
    delta = int(float(mission_end) - float(mission_start))
    min, sec = divmod(delta, 60)

    if min > 0:
        print(
            colored("\n[Status] Trial lasted for: ", "blue", attrs=["bold"]),
            min,
            "Minutes",
            sec,
            "Seconds",
        )
    else:
        print(
            colored("\n[Error] Timing for mission is off by a large margin", "red"),
            "\N{cross mark}",
        )
        logging.info(
            "\tMinecraft\tTime_taken_for_mission_out_of_range\t{}".format(path)
        )


def read_subject_id(TrialMessages, logging):

    global training_counter
    global saturn_A_counter
    global saturn_B_counter

    print(colored("\n[Status] Minecraft mission info:", "blue", attrs=["bold"]))
    for i in range(len(TrialMessages)):
        try:
            print(
                colored("\n[Info] Mission name:", "blue", attrs=["bold"]),
                TrialMessages[i]["data"]["experiment_mission"],
            )

            """
            Keep a counter for every mission as sometimes metadata can have multiple trials under same trial ID. 
            """
            if TrialMessages[i]["data"]["experiment_mission"] == "Hands-on Training":
                training_counter += 1
            if TrialMessages[i]["data"]["experiment_mission"] == "Saturn_A":
                saturn_A_counter += 1
            if TrialMessages[i]["data"]["experiment_mission"] == "Saturn_B":
                saturn_B_counter += 1

            if training_counter > 1:
                print(
                    colored(
                        "\n[Warning] Multiple Hands-on Training missions found",
                        "blue",
                        attrs=["bold"],
                    ),
                )
                logging.info("\tMinecraft\tMultiple_Handson_Training_missions_found")
            elif saturn_A_counter > 1:
                print(
                    colored(
                        "\n[Warning] Multiple Saturn_A missions found",
                        "blue",
                        attrs=["bold"],
                    ),
                )
                logging.info("\tMinecraft\tMultiple_Saturn_A_missions_found")
            elif saturn_B_counter > 1:
                print(
                    colored(
                        "\n[Warning] Multiple Saturn_B missions found",
                        "blue",
                        attrs=["bold"],
                    ),
                )
                logging.info("\tMinecraft\tMultiple_Saturn_B_missions_found")

            if training_counter >= 2 or saturn_A_counter >= 2 or saturn_B_counter >= 2:
                training_counter, saturn_A_counter, saturn_B_counter = 0, 0, 0

            print(
                colored("\n[Info] Testbed version:", "blue", attrs=["bold"]),
                TrialMessages[i]["data"]["testbed_version"],
            )

            print(colored("\n[Info] Subject info:", "blue", attrs=["bold"]))
            for idx, sub in enumerate(TrialMessages[i]["data"]["subjects"]):
                print(colored("\t Subect ID", "magenta"), idx, ":", sub)
            if idx == 2:
                break
        except:
            continue


def read_metadata_as_json(path, logging):
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
                        condition.append("Start")
                    if json_message["data"]["mission_state"] == "Stop":
                        mission_end = json_message["msg"]["timestamp"]
                        condition.append("Stop")

                if len(condition) == 2:
                    continue
                else:
                    """
                    There can be issue where the experimentor ends the mission
                    abruptly, then mission_state wouldn't have stop message.
                    Switch to trial message instead and see when the trial
                    ended and use that as mission_end.
                    """
                    if json_message["header"]["message_type"] == "trial":
                        if json_message["msg"]["sub_type"] == "start":
                            print(
                                colored(
                                    "\n[Warning] Mission start not found. Using trial start as mission start!",
                                    "yellow",
                                )
                            )
                            mission_start = json_message["msg"]["timestamp"]
                            logging.info(
                                "\tMinecraft\tUsed_trial_start_as_mission_start_not_found\t{}".format(
                                    path
                                )
                            )

                        if json_message["msg"]["sub_type"] == "stop":
                            print(
                                colored(
                                    "\n[Warning] Mission end not found. Using trial end as mission end!",
                                    "yellow",
                                )
                            )
                            mission_end = json_message["msg"]["timestamp"]
                            logging.info(
                                "\tMinecraft\tUsed_trial_end_as_mission_end_not_found\t{}".format(
                                    path
                                )
                            )

            except:
                print(
                    colored("\n[Error] Cannot read JSON line", "red"),
                    "\N{cross mark}",
                )
                logging.info(
                    "\tMinecraft\tCannot_read_metadata_as_JSON\t{}".format(path)
                )

    read_subject_id(TrialMessages, logging)
    check_time_difference(mission_start, mission_end, logging, path)


def checkfile_minecraft(rootdir, logging):
    """
    This function checks if the .metadata file is present under the given path or not. It also checks
    if the .metadata file is empty or not.
    """

    rootdir = os.path.join(rootdir, "minecraft")
    if os.path.exists(rootdir):
        #check if the minecraft folder exist or not
        dir = os.listdir(rootdir)
    
        if len(dir) == 0:
            # sometime .DS_Store might be there so the length would be 1
            print(
                colored("\n[Error] Metadata file is missing", "red", attrs=["bold"]),
                "\N{cross mark}",
            )
            logging.info("\tMinecraft\tMetadata_missing")

        else:
            for x in os.listdir(rootdir):
                if x.endswith(".metadata"):
                    # check if .metadata file exists or not
                    print(
                        colored("\n[Status] Metadata File:", "blue", attrs=["bold"]),
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
                        logging.info(
                            "\tMinecraft\tMetadata_is_empty\t{}".format(
                                os.path.join(rootdir, x)
                            )
                        )
                    else:
                        read_metadata_as_json(os.path.join(rootdir, x), logging)

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
                    logging.info("\tMinecraft\tMetadata_missing")                                               

    else:
        print(
        colored("\n[Error] Minecraft folder is missing", "red", attrs=["bold"]),
        "\N{cross mark}",
        )
        logging.info("\tMinecraft\tMinecraft_folder_missing")      
