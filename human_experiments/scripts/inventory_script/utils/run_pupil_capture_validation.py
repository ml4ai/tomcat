import os
import os.path
from termcolor import colored

pupil_file_list_1 = [
    "eye1.mp4",
    "eye0.mp4",
    "world.mp4",
    "surfaces.pldata",
    "world_timestamps.npy",
    "notify.pldata",
    "blinks_timestamps.npy",
    "gaze_timestamps.npy",
    "notify_timestamps.npy",
    "user_info.csv",
    "gaze.pldata",
    "eye0.intrinsics",
    "eye1_timestamps.npy",
    "pupil.pldata",
    "eye0_timestamps.npy",
    "info.player.json",
    "surface_definitions_v01",
    "surfaces_timestamps.npy",
    "eye1.intrinsics",
    "blinks.pldata",
    "fixations_timestamps.npy",
    "pupil_timestamps.npy",
    "world.intrinsics",
    "fixations.pldata",
]
pupil_file_list_2 = [
    "eye1.mp4.writing",
    "eye0.mp4.writing",
    "world.mp4.writing",
    "surfaces.pldata",
    "world_timestamps.npy",
    "notify.pldata",
    "blinks_timestamps.npy",
    "gaze_timestamps.npy",
    "notify_timestamps.npy",
    "user_info.csv",
    "gaze.pldata",
    "eye0.intrinsics",
    "eye1_timestamps.npy",
    "pupil.pldata",
    "eye0_timestamps.npy",
    "info.player.json",
    "surface_definitions_v01",
    "surfaces_timestamps.npy",
    "eye1.intrinsics",
    "blinks.pldata",
    "fixations_timestamps.npy",
    "pupil_timestamps.npy",
    "world.intrinsics",
    "fixations.pldata",
]


def check_pupil_recorder(rootdir_master, logging, imac):
    rootdir = rootdir_master + imac + "/pupil_recorder/000/"
    for f1, f2 in zip(pupil_file_list_1, pupil_file_list_2):
        if os.path.isfile(rootdir + f1) or os.path.isfile(rootdir + f2):
            if ".writing" in rootdir + f2:
                # check *.mp4.writing files
                try:
                    if os.stat(rootdir + f2).st_size != 0:
                        print(
                            colored(
                                "\n [Warning] Incomplete Pupil capture file found at ",
                                "yellow",
                                attrs=["bold"],
                            ),
                            colored(rootdir + f2, "cyan"),
                        )
                        logging.info(
                            "\tPupil_recorder\tFile_didnt_complete_Writing\t{}".format(
                                rootdir + f2
                            )
                        )
                    else:
                        print(
                            colored(
                                "\n [Error] Incomplete and empty Pupil capture file found at ",
                                "red",
                                attrs=["bold"],
                            ),
                            colored(rootdir + f2, "cyan"),
                            "\N{cross mark}",
                        )
                        logging.info(
                            "\tPupil_recorder\tIncomplete_file_found_but_no_data\t{}".format(
                                rootdir + f2
                            )
                        )
                except:
                    pass
            else:
                # check *.mp4 files
                if os.stat(rootdir + f1).st_size != 0:
                    print(
                        colored(
                            "\n [Status] Pupil capture file found at ",
                            "blue",
                            attrs=["bold"],
                        ),
                        colored(rootdir + f1, "cyan"),
                    )
                else:
                    print(
                        colored(
                            "\n [Error] Empty Pupil capture file found at ",
                            "red",
                            attrs=["bold"],
                        ),
                        colored(rootdir + f1, "cyan"),
                        "\N{cross mark}",
                    )
                    logging.info(
                        "\tPupil_recorder\tFile_found_but_no_data\t{}".format(
                            rootdir + f1
                        )
                    )
        else:
            print(
                colored("\n [Error] Pupil capture file: ", "red", attrs=["bold"]),
                colored(rootdir + f1, "yellow"),
                colored("not found! ", "red", attrs=["bold"]),
                "\N{cross mark}",
            )
            logging.info("\tPupil_recorder\tFile_not_found\t{}".format(rootdir + f1))
