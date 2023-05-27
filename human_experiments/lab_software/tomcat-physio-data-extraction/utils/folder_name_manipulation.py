import os 

def get_new_file_paths(output_path, csv_file_name):
    # Get common prefix
    common_prefix = os.path.commonprefix([output_path, csv_file_name])

    # Get the unique part of csv_file_name
    unique_path = csv_file_name.replace(common_prefix, "").strip("/")

    # Ignore 'eeg_fnirs_pupil' in the path
    unique_path = unique_path.replace("eeg_fnirs_pupil", "").strip("/")

    # Now append unique_path to output_path
    new_csv_file_path = os.path.join(output_path, unique_path)
    new_csv_file_path_filtered = os.path.join(output_path, unique_path)

    return new_csv_file_path, new_csv_file_path_filtered
