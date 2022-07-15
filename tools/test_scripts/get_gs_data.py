#!/usr/bin/env python3

import argparse
import subprocess
import sys
import json

# Authors:  Joseph Astier, Adarsh Pyarelal

# Download testbed data from the Google Cloud and validate using the Testbed 
# test script for this agent

BUCKET = 'studies.aptima.com/study-3_2022'

def get_gs_filenames(gs_query):
    gs_filenames = []
    proc = subprocess.run(['gsutil', 'ls', gs_query], capture_output=True)
    if proc.returncode == 0:
        output=proc.stdout.decode('utf-8')
        output_lines = output.split('\n')
        for line in output_lines:
            if len(line) > 0:
                gs_filenames.append(line)

    return gs_filenames


# Download all .metadata files in a single dataset into a local
# directory of the same name.  Existing directories are clobbered.
def download_dataset(dataset_name):

    # Delete the local dataset directory if it exists
    proc = subprocess.run(['rm','-rf', dataset_name])
    if not proc.returncode == 0:
        print(f'Could not delete directory {dataset_name}')
        print(proc.stderr.decode('utf-8'))
        return

    # Create the local dataset directory
    proc_ = subprocess.run(['mkdir', dataset_name])
    if not proc.returncode == 0:
        print(f'Could not create directory {dataset_name}')
        print(proc.stderr.decode('utf-8'))
        return

    # List the files in the cloud dataset
    gs_query = f'gs://{BUCKET}/*{dataset_name}*.metadata'
    gs_filenames = get_gs_filenames(gs_query)
    n_filenames = len(gs_filenames)
    if n_filenames == 0:
        print(f'{dataset_name}: No data files found.')
        return

    if n_filenames == 1:
        print(f'{dataset_name}: Downloading 1 data file...')
    else:
        print(f'{dataset_name}: Downloading {n_filenames} data files...')

    # Download the files from the cloud dataset
    dataset_dir = f'./{dataset_name}'
    proc = subprocess.run(['gsutil', '-m', 'cp', gs_query, dataset_dir])
    if not proc.returncode == 0:
        print(f'Could not download {gs_filename}')
        print(proc.stderr.decode('utf-8'))

# download metadata studies and test files
if __name__ == '__main__':

    parser = argparse.ArgumentParser(
        description='Google cloud storage Testbed dataset downloader')

    parser.add_argument(
        'datasets', 
        action='store', 
        nargs = '+',
        help = 'One or more dataset names, e.g. \"TM000nnn\"')

    args = parser.parse_args(sys.argv[1:])

    # Download all the Testbed datasets specified by the user
    for dataset in args.datasets:
        download_dataset(dataset)
