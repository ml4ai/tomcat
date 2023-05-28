import os 

def get_new_file_paths(output_path, csv_file_name):
    # Ensure csv_file_name is a relative path
    csv_file_name = csv_file_name.lstrip("/")
    
    # Ignore 'eeg_fnirs_pupil' in the path
    csv_file_name = csv_file_name.replace("eeg_fnirs_pupil/", "")
    
    # Now append csv_file_name to output_path
    new_csv_file_path = os.path.join(output_path, csv_file_name)
    
    return new_csv_file_path