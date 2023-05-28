#!/bin/bash

# Define an array of directories to ignore
ignore=("exp_2022_04_01_13" "exp_2022_04_22_09" "exp_2023_04_17_13" "exp_2023_04_18_14" "exp_2023_04_20_14" "exp_2023_04_21_10" "exp_2023_04_24_13" "exp_2023_04_27_14" "exp_2023_04_28_10" "exp_2023_05_01_13" "exp_2023_05_02_14" "exp_2023_05_03_10")

while getopts ":d:o:" opt; do
  case $opt in
    d) rootdir="$OPTARG"
    ;;
    o) outputdir="$OPTARG"
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
    local str_array=("$2" "$3" "$4")
    # local experiment_output_dir="$outputdir/data/${base_dir}" #for local /data is needed/
    local experiment_output_dir="$outputdir/${base_dir}"

    # Create the directory if it does not exist
    mkdir -p "$experiment_output_dir"

    local error_log="$experiment_output_dir/error_log.txt"

    # Call python script and save stderr to error log
    if ! python3 run_physio_data_extraction.py --p1 "$dir" --p2 "$dir/baseline_tasks/" --p3 "$dir/minecraft/" --s "${str_array[0]}" --s "${str_array[1]}" --s "${str_array[2]}" --filter True --hdf5 True --output_path "$outputdir" 2>> "$error_log"; then
        echo "Python script failed for $dir."
    fi
}

for dir in "${directories[@]}"; do
    # Extract the base directory name
    max_jobs=6
    active_jobs=0

    base_dir=$(basename "$dir")

    # Check if directory is in ignore list
    if [[ " ${ignore[@]} " =~ " ${base_dir} " ]]; then
        # If directory is in ignore list, skip to next iteration
        continue
    fi

    # Find all csv files in the specific directory
    csv_files=($(find "$dir/baseline_tasks/affective" -type f -name "individual_*.csv"))

    # Create an array to store the extracted str_array values
    str_array=()

    for file in "${csv_files[@]}"; do
        # Extract the part of the filename after the underscore
        str_array+=("$(basename "$file" | sed -e 's/individual_//' -e 's/_.*//' -e 's/.csv//')")
    done
    
    # Print the command before executing
    echo "Running: python3 run_physio_data_extraction.py --p1 \"$dir\" --p2 \"$dir/baseline_tasks/\" --p3 \"$dir/minecraft/\" --s \"${str_array[0]}\" --s \"${str_array[1]}\" --s \"${str_array[2]}\" --filter True --hdf5 True --output_path \"$outputdir\""

    # Start a separate process to execute the Python script
    execute_python_script "$dir" "${str_array[0]}" "${str_array[1]}" "${str_array[2]}" &

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
