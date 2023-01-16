import os
from termcolor import colored

def check_xdf(rootdir_master, logging, imac):
    rootdir = rootdir_master + "/" + imac + "/eeg_fnirs_pupil/"
    for root, dirs, files in os.walk(rootdir):
        for file in files:
            try:
                if file.endswith(".xdf"):
                    if os.stat(root + "/" + file).st_size != 0:
                        print(
                            colored(
                                "\n [Status] xdf file found at ", "blue", attrs=["bold"]
                            ),
                            colored(os.path.join(root, file), "cyan"),
                        )
                    else:
                        print(
                            colored(
                                "[Error] xdf file found but is empty ",
                                "red",
                                attrs=["bold"],
                            ),
                        )
                        logging.info(
                            "\XDF_file\File_found_but_empty\t{}".format(
                                os.path.join(root, file)
                            )
                        )
            except:
                print(
                    colored("[Error] xdf file is not present", "red", attrs=["bold"]),
                )
                logging.info(
                    "\XDF_file\tFile_not_found\t{}".format(os.path.join(root, file))
                )
