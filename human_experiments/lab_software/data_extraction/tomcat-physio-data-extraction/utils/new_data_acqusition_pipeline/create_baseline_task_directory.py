import os
from termcolor import colored

def create_baseline_task_directory(input_path, output_path):
    # Extract experiment name from input path
    exp_name = os.path.basename(input_path)

    # Define subdirectories
    subdirs = ["affective", "ping_pong", "rest_state", "finger_tapping"]

    print(
        colored(
            "[Status] Creating baseline task directories for experiment ",
            "green",
            attrs=["bold"],
        )
    )

    # Loop over each subdirectory and create it
    for subdir in subdirs:
        # Full directory path
        dir_path = os.path.join(output_path, exp_name, "baseline_tasks", subdir)

        # Create directory
        os.makedirs(dir_path, exist_ok=True)
