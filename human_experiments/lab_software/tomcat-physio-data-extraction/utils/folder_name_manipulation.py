import os 

def get_new_file_paths(output_path, csv_file_name):
    # Ensure csv_file_name is a relative path
    csv_file_name = csv_file_name.lstrip("/")
    
    # Split the path into its components
    path_parts = csv_file_name.split("/")
    
    # Find the index of the part of the path we're interested in
    try:
        start_index = path_parts.index('study_3_pilot')
    except ValueError:
        raise ValueError("The csv file path must contain 'study_3_pilot'")
    
    # Include only the parts of the path after 'study_3_pilot'
    csv_file_name = "/".join(path_parts[start_index + 1:])
    
    # Ignore 'eeg_fnirs_pupil' in the path
    csv_file_name = csv_file_name.replace("eeg_fnirs_pupil/", "")
    
    # Now append csv_file_name to output_path
    new_csv_file_path = os.path.join(output_path, csv_file_name)
    
    return new_csv_file_path