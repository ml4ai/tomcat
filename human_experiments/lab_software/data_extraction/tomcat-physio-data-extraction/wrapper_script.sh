#!/bin/bash

# Define an array of directories to ignore
ignore=("exp_2022_04_01_13" "exp_2022_04_22_09")

data_validity=""

while getopts ":d:o:v:" opt; do
  case $opt in
    d) rootdir="$OPTARG"
    ;;
    o) outputdir="$OPTARG"
    ;;
    v) data_validity="$OPTARG"
    ;;
    \?) echo "Invalid option -$OPTARG" >&2
    ;;
  esac
done

# Check if root directory and output directory were provided
if [ -z "$rootdir" ] || [ -z "$outputdir" ]; then
  echo "You must provide a root directory with the -d flag and an output directory with the -o flag"
  exit 1
fi

# Search for directories starting with exp_ under the root directory
directories=($(ls -d $rootdir/exp_*))

# Define the function to execute the Python script
execute_python_script() {
    local dir="$1"
    local experiment_output_dir="$outputdir/${base_dir}"

    # Create the directory if it does not exist
    mkdir -p "$experiment_output_dir"

    local error_log="$experiment_output_dir/error_log.txt"

    # Call python script and save stderr to error log
    if ! python3 run_physio_data_extraction.py --p "$dir" --csv True --output_path "$outputdir" --data_validity "$data_validity" 2>> "$error_log"; then
        echo "Python script failed for $dir."
    fi
}

for dir in "${directories[@]}"; do
    # Extract the base directory name
    max_jobs=12
    active_jobs=0

    base_dir=$(basename "$dir")

    # Check if directory is in ignore list
    if [[ " ${ignore[@]} " =~ " ${base_dir} " ]]; then
        # If directory is in ignore list, skip to next iteration
        continue
    fi

    # Print the command before executing
    echo "Running: python3 run_physio_data_extraction.py --p \"$dir\" --csv True --hdf5 True --output_path \"$outputdir\" --data_validity \"$data_validity\""

    # Start a separate process to execute the Python script
    execute_python_script "$dir" &

    # Count the number of active jobs
    active_jobs=$((active_jobs+1))

    # If we've hit the maximum number of jobs, wait until they're all done
    if [[ active_jobs -ge max_jobs ]]; then
        wait  # Wait for all background jobs to finish
        active_jobs=0  # Reset the job count
    fi

done

# Wait for all child processes to finish
wait
