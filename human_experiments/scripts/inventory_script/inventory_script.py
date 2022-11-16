from itertools import count
import sys  # We use sys.exit() so that all exceptions can properly propogate up and cause the interpreter to exit.
import os
import json
import argparse
import datetime
from termcolor import colored
from utils import checkfile_minecraft, fcount_baseline_task, \
                  check_audio, check_vocalics, check_xdf, \
                  check_pupil_recorder, check_asist_folder

def inventory_script(rootdir):
    dir = os.listdir(rootdir)

    if len(dir) == 0:
        # sometime .DS_Store might be there so the length would be 1
        print(
            colored(
                "\n[Error] Root directory file is missing", "red", attrs=["bold"]
            ),
            "\N{cross mark}",
        )
    else:
        fcount_baseline_task(rootdir+'baseline_tasks')
        checkfile_minecraft(rootdir+'minecraft')
        check_xdf(rootdir)
        check_pupil_recorder(rootdir)
        check_audio(rootdir, 'lion')
        check_audio(rootdir, 'tiger')
        check_audio(rootdir, 'leopard')
        check_asist_folder(rootdir)
        check_vocalics(rootdir)
            
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Inventory Script that checks for metadata files from baseline task and minecraft"
    )
    parser.add_argument(
        "--p",
        required=True,
        help="Path to experiment folder",
    )

    arg = parser.parse_args()
    rootdir = arg.p
    print(
        colored("[Status] Root Directory:", "blue", attrs=["bold"]),
        colored(rootdir, "cyan"),
    )
    sys.exit(inventory_script(rootdir))
