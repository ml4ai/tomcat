import os
import sys
import pyxdf
import shutil
import argparse
import numpy as np
from termcolor import colored
from utils import (
    str2bool,
    read_xdf_old,
    read_xdf_new,
)

def look_for_XDF_files(
    rootdir_xdf,
    extract_pkl,
    extract_csv,
    extract_hdf5,
    exclude,
    output_path,
):
    """
    Walk through root directory, looking for the xdf files.
    """
    xdf_file_paths = [
        os.path.join(root, file)
        for root, dirs, files in os.walk(rootdir_xdf)
        for file in files
        if file.endswith(".xdf")
    ]

    for file_path in xdf_file_paths:
        print(
            colored("[Status] xdf file found at ", "green", attrs=["bold"]),
            colored(file_path, "blue"),
        )
    if len(xdf_file_paths) == 0:
        print(
            colored("[Status] No xdf files found at ", "red", attrs=["bold"]),
            colored(rootdir_xdf, "blue"),
        )
        sys.exit()
    elif len(xdf_file_paths) > 2:
        print(
            colored("[Status] +++++ Extracting data for experiments before 2023_04_17 ++++++", "green", attrs=["bold"])
        )
        read_xdf_old(
            sorted(xdf_file_paths),
            extract_pkl,
            extract_csv,
            extract_hdf5,
            exclude,
            output_path,
        )  # 1. read all the XDF files

    elif len(xdf_file_paths) == 2:
        print(
            colored("[Status] +++++ Extracting data for experiments after 2023_04_17 ++++++", "green", attrs=["bold"])
        )
        read_xdf_new(
            sorted(xdf_file_paths),
            extract_pkl,
            extract_csv,
            extract_hdf5,
            exclude,
            output_path,
            rootdir_xdf,
        )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Post experiment script for xdf to csv file conversion"
    )
    parser.add_argument(
        "--p", required=True, help="Path to the directory with the XDF files"
    )

    parser.add_argument(
        "--pkl",
        default=False,
        type=str2bool,
        help="By setting pkl to True you extract xdf files as pickle file",
    )

    parser.add_argument(
        "--csv",
        default=True,
        type=str2bool,
        help="By setting csv to True you extract xdf files as csv file",
    )

    parser.add_argument(
        "--hdf5",
        default=False,
        type=str2bool,
        help="By setting hdf5 to True you extract xdf files as hdf5 file",
    )

    parser.add_argument(
        "--exclude",
        required=False,
        default=None,
        help="Enter iMAC name you'd like to exclude",
    )

    parser.add_argument(
        "--output_path",
        required=False,
        default=None,
        help="Enter path to a folder where you want to extract the physio data, by default \
            it will go to same folder your reading XDF files from",
    )

    arg = parser.parse_args()

    rootdir_xdf = arg.p
    extract_pkl = arg.pkl
    extract_hdf5 = arg.hdf5
    extract_csv = arg.csv
    exclude = str(arg.exclude)
    output_path = arg.output_path

    print(
        colored("[Status] Root Directory:", "green", attrs=["bold"]),
        colored(rootdir_xdf, "blue"),
    )
    sys.exit(
        look_for_XDF_files(
            rootdir_xdf,
            extract_pkl,
            extract_csv,
            extract_hdf5,
            exclude,
            output_path,
        )
    )
