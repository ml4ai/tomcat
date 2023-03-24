import sys  # Use sys.exit() so that all exceptions can properly propogate up and cause the interpreter to exit.
import os
import argparse
from termcolor import colored
from utils import (
    checkfile_minecraft,
    fcount_baseline_task,
    check_audio,
    check_vocalics,
    check_xdf,
    check_pupil_recorder,
    check_asist_folder,
    inventory_script_logging,
)

def inventory_script(rootdir, path_out):
    if path_out == None:
        logging_path = os.path.join(rootdir, "sanity_check")
        logging_path_flag = os.path.exists(logging_path)
    else:
        try:
            logging_path = (
                path_out + "/sanity_check/" + "exp_" + rootdir.split("exp_", 1)[1]
            )
        except:
            logging_path = (
                path_out + "/sanity_check/" + "exp_" + rootdir.split("exp_", 1)[0]
            )

        logging_path_flag = os.path.exists(logging_path)

    if not logging_path_flag:
        os.makedirs(logging_path)

    print(logging_path_flag)

    logger = inventory_script_logging(os.path.join(logging_path, "missing_data.log"))
    logger.debug("Data_type-Issue-file_path")

    dir = os.listdir(rootdir)
    if len(dir) == 0:
        # sometime .DS_Store might be there so the length would be 1
        print(
            colored("\n[Error] Root directory file is missing", "red", attrs=["bold"]),
            "\N{cross mark}",
        )
        logger.info("\tRoot_directory\Does_not_exist\t{}".format(rootdir))

    else:

        fcount_baseline_task(rootdir, logger)
        checkfile_minecraft(rootdir, logger)
        check_xdf(rootdir, logger, "lion")
        check_xdf(rootdir, logger, "tiger")
        check_xdf(rootdir, logger, "leopard")
        check_pupil_recorder(rootdir, logger, "lion")
        check_pupil_recorder(rootdir, logger, "tiger")
        check_pupil_recorder(rootdir, logger, "leopard")
        check_audio(rootdir, logger, "lion")
        check_audio(rootdir, logger, "tiger")
        check_audio(rootdir, logger, "leopard")
        check_asist_folder(rootdir, logger)
        check_vocalics(rootdir, logger)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Inventory Script that checks for metadata files from baseline task and minecraft"
    )

    parser.add_argument(
        "--wrapper",
        required=False,
        default=None,
        help="Wrapper script that runs inventory script over all exp_* folders.",
    )

    parser.add_argument(
        "--p",
        required=True,
        help="Path to single experiment folder. If wrapper script is set to true then add path that contains all exp_* folders. ",
    )

    parser.add_argument(
        "--path_out",
        required=False,
        default=None,
        help="Path to external folder to place sanity check logs",
    )

    arg = parser.parse_args()
    rootdir = arg.p
    path_out = arg.path_out
    wrapper = arg.wrapper

    if bool(wrapper) == True:
        """
        This section requires wrapper argument to be true and path to subdirectory that has all exp_* folders.
        """
        print(
            colored("[Status] Root Directory:", "blue", attrs=["bold"]),
            colored(rootdir, "cyan"),
        )

        for dirs in os.walk(rootdir):
            # Get a list of all exp_* folder under the path passed as argument
            dir = dirs[1]
            break

        for rootdirec in dir:
            # Run inventory_script over all exp_* folders.
            print(
                colored("[Status] Sub Directory:", "blue", attrs=["bold"]),
                colored(os.path.join(rootdir, rootdirec), "cyan"),
            )
            if 'exp_2022_04_01_13' or 'exp_2022_04_22_09' in rootdir:
                print(
                    colored("[Status] Skipping Sub Directory:", "yellow", attrs=["bold"]),
                    colored(os.path.join(rootdir, rootdirec), "cyan"),
                )
            else:
                inventory_script(os.path.join(rootdir, rootdirec), path_out)

    else:
        sys.exit(inventory_script(rootdir, path_out))
