#!/usr/bin/env python3

import argparse
import subprocess
import sys
import json

# Authors:  Joseph Astier, Adarsh Pyarelal

# List the filenames in the Google Cloud for this query
def get_gs_filenames(gs_query):
    proc = subprocess.run(['gsutil', 'ls', gs_query], capture_output=True)
    if not proc.returncode == 0:
        print(f'Problem reading query: {gs_querr}')
        print(proc.stderr.decode('utf-8'))
        return []

    output = proc.stdout.decode('utf-8')
    lines = output.split('\n')
    # return .metadata files
    return list(filter(lambda x: x.endswith('.metadata'), lines))


# Download all .metadata files in a single dataset into a local
# directory of the same name.  Existing directories are clobbered.
def download_dataset(dataset_name, args):

    # Delete the local dataset directory always
    proc = subprocess.run(['rm','-rf', dataset_name])
    if not proc.returncode == 0:
        print(f'Could not delete directory {dataset_name}')
        print(proc.stderr.decode('utf-8'))
        return 0

    # List the files in the cloud dataset
    gs_query = f'gs://{args.bucket}/*{dataset_name}*.metadata'
    gs_filenames = get_gs_filenames(gs_query)
    n_filenames = len(gs_filenames)
    if n_filenames == 0:
        print(f'{dataset_name}: No data files found.')
        return 0

    # Create the local dataset directory
    proc_ = subprocess.run(['mkdir', dataset_name])
    if not proc.returncode == 0:
        print(f'Could not create directory {dataset_name}')
        print(proc.stderr.decode('utf-8'))
        return 0

    if n_filenames == 1:
        print(f'{dataset_name}: Downloading 1 data file...')
    else:
        print(f'{dataset_name}: Downloading {n_filenames} data files...')

    # Download the files from the cloud dataset
    dataset_dir = f'./{dataset_name}'
    proc = subprocess.run(['gsutil', '-m', 'cp', gs_query, dataset_dir])
    if not proc.returncode == 0:
        print(f'Could not download {dataset_name}')
        print(proc.stderr.decode('utf-8'))
        return 0

    return n_filenames

# download metadata studies and test files
if __name__ == '__main__':

    parser = argparse.ArgumentParser(
        description=(
            'Google cloud storage Testbed dataset downloader.  This script '
            'will download Testbed datasets (TM000nnn) into local '
            'directories of the same name.'
        )
    )

    parser.add_argument(
        '-b',
        '--bucket',
        default = 'studies.aptima.com/study-3_2022',
        help = 'Name of Good Cloud bucket'
    )
    parser.add_argument(
        'datasets', 
        action='store', 
        nargs = '+',
        help = 'One or more dataset names, e.g. \"TM000nnn\"'
    )

    args = parser.parse_args(sys.argv[1:])

    # Download all the Testbed datasets specified by the user
    n_files_downloaded = 0
    for dataset in args.datasets:
        n_files_downloaded += download_dataset(dataset, args)

    print(f'Files downloaded: {n_files_downloaded}')
