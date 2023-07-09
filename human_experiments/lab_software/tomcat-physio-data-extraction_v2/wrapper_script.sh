#!/bin/bash

# Initialize our own variables:
output_path=""
input_path=""

# Process flags
while getopts "i:o:" opt; do
  case "${opt}" in
    i) input_path="${OPTARG}" ;;
    o) output_path="${OPTARG}" ;;
  esac
done

# Check if output_path and input_path are provided
if [ -z "${output_path}" ] || [ -z "${input_path}" ]; then
    echo "Both input_path (-i) and output_path (-o) must be provided."
    exit 1
fi

error_log="${output_path}/error.log"

# Create an array of directories modified after 04/17/23
directories=($(find ${input_path} -type d -newermt '2023-04-17'))

# Export variables to make them available in GNU Parallel
export error_log output_path

# Define python command to be run on each directory
cmd() {
    directory=$1
    python3 run_physio_data_extraction_v2.py -p "${directory}" --output_path "${output_path}" --filter True --hdf5 True 2>> "${error_log}"
}

# Export the command as a function for GNU Parallel
export -f cmd

# Use GNU Parallel to run the command concurrently on all directories
parallel -j $(nproc) cmd ::: "${directories[@]}"
