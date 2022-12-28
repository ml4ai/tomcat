import os
import sys
import pyxdf
import shutil
import argparse
from termcolor import colored
from utils import get_start_stop_time_from_xdf, dataframe_to_csv, create_time_distribution, str2bool

def read_xdf(xdf_file_paths, rootdir_baseline_task, rootdir_minecraft_data, subject_id, extract_pkl, extract_csv, exclude, filter):
    """
    Read the XDF files.
    """
    columns = shutil.get_terminal_size().columns
    for path in xdf_file_paths:
        data, header = pyxdf.load_xdf(path)
        
        if exclude not in path:             
            if 'lion' in path: print(colored('Lion ', 'magenta', attrs=['bold', 'blink']).center(columns))
            elif 'leopard' in path: print(colored('Leopard ', 'magenta', attrs=['bold', 'blink']).center(columns))
            else: print(colored('Tiger ', 'magenta', 'on_blue', attrs=['bold', 'blink']).center(columns))

            for i in range(0,len(data)):
                if data[i]['info']['type'] == ['NIRS']:
                    print(
                    colored('[Status] Reading ', 'green', attrs=['bold']), 
                    colored(data[i]['info']['type'], 'blue'))
                    time_start_streams_nirs, time_end_streams_nirs = get_start_stop_time_from_xdf(data[i]) #get the unix time
                    time_distribution_human_readable_nirs, time_distribution_unix_nirs = create_time_distribution(time_start_streams_nirs, 
                                                                                time_end_streams_nirs, len(data[i]['time_series']))
                    dataframe_to_csv(path, data[i]['time_series'], 'NIRS', time_distribution_human_readable_nirs, time_distribution_unix_nirs, 
                                    rootdir_baseline_task, rootdir_minecraft_data, subject_id, extract_pkl, extract_csv, filter)

                # elif data[i]['info']['type'] == ['Markers']:
                #     #We don't have physical marker for our physio data
                #     print(
                #     colored('[Status] Skipping ', 'green', attrs=['bold']), 
                #     colored(data[i]['info']['type'], 'blue'))                

                # elif data[i]['info']['type'] == ['EEG']:
                #     print(
                #     colored('[Status] Reading ', 'green', attrs=['bold']), 
                #     colored(data[i]['info']['type'], 'blue'))
                #     time_start_streams_eeg, time_end_streams_eeg = get_start_stop_time_from_xdf(data[i]) #get the unix time
                #     time_distribution_human_readable_eeg, time_distribution_unix_eeg = create_time_distribution(time_start_streams_eeg, 
                #                                                                 time_end_streams_eeg, len(data[i]['time_series'])) 
                #     dataframe_to_csv(path, data[i]['time_series'], 'EEG', time_distribution_human_readable_eeg, 
                #     time_distribution_unix_eeg, rootdir_baseline_task, rootdir_minecraft_data, subject_id, extract_pkl, extract_csv, filter)

                # elif data[i]['info']['type'] == ['Gaze']:
                #     print(
                #     colored('[Status] Reading ', 'green', attrs=['bold']), 
                #     colored(data[i]['info']['type'], 'blue'))
                #     time_start_streams_gaze, time_end_streams_gaze = get_start_stop_time_from_xdf(data[i]) #get the unix time
                #     time_distribution_human_readable_gaze, time_distribution_unix_gaze = create_time_distribution(time_start_streams_gaze, 
                #                                                                 time_end_streams_gaze, len(data[i]['time_series'])) 
                #     dataframe_to_csv(path, data[i]['time_series'], 'Gaze', time_distribution_human_readable_gaze, 
                #     time_distribution_unix_gaze, rootdir_baseline_task, rootdir_minecraft_data, subject_id, extract_pkl, extract_csv, filter)
                
                # elif data[i]['info']['type'] == ['Accelerometer']:
                #     print(
                #     colored('[Status] Skipping ', 'green', attrs=['bold']), 
                #     colored(data[i]['info']['type'], 'blue')) 
                #     #create_csv_file(path, 'Accelerometer')
                #     # time_start_streams_accel, time_end_streams_accel = get_start_stop_time_from_xdf(data[i]) #get the unix time
        else:
            print(
            colored('[Status] Skipping ', 'yellow', attrs=['bold']), 
            colored(exclude, 'red'))         

def look_for_XDF_files(rootdir_xdf, rootdir_baseline_task, rootdir_minecraft_data, subject_id, extract_pkl, extract_csv, exclude, filter):
    """
    Walk through root directory, looking for the xdf files. 
    """
    xdf_file_paths = []
    for root, dirs, files in os.walk(rootdir_xdf):
        for file in files:
            if file.endswith(".xdf"):
                xdf_file_paths.append(os.path.join(root, file))
                print(
                    colored('[Status] xdf file found at ', 'green', attrs=['bold']), 
                    colored(os.path.join(root, file), 'blue'))
    
    read_xdf(sorted(xdf_file_paths), rootdir_baseline_task, rootdir_minecraft_data, subject_id, extract_pkl, extract_csv, exclude, filter) #1. read all the XDF files 

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Post experiment script for xdf to csv file conversion"
    )
    parser.add_argument(
        "--p1",
        required=True,
        help="Path to the directory with the XDF files")

    parser.add_argument(
        "--p2", 
        required=True, 
        help="Enter the Path to folder with baseline task data")

    parser.add_argument(
        "--p3", 
        required=True, 
        help="Enter the Path to folder with minecraft data")
    
    parser.add_argument(
        '--s',
        required=True, action='append', 
        help="Enter the Path to folder with baseline task data")

    parser.add_argument(
        '--pkl',
        default=False, 
        type=str2bool,
        help="By setting pkl to True you extract xdf files as pickle file")

    parser.add_argument(
        '--csv',
        default=True, 
        type=str2bool, 
        help="By setting csv to True you extract xdf files as csv file")

    parser.add_argument(
        '--exclude',
        required=False,
        default=None,
        help="Enter iMAC name you'd like to exclude")

    parser.add_argument(
        '--filter',
        required=False,
        default=None,
        help="Enter True if you want to filter the siganl")

    arg = parser.parse_args()

    rootdir_xdf = arg.p1
    rootdir_baseline_task = arg.p2
    rootdir_minecraft_data = arg.p3
    subject_id = arg.s
    extract_pkl = arg.pkl
    extract_csv = arg.csv
    exclude  = str(arg.exclude)
    filter = str(arg.filter)

    print(colored('[Status] Root Directory:', 'green', attrs=['bold']), colored(rootdir_xdf, 'blue'))
    sys.exit(look_for_XDF_files(rootdir_xdf, rootdir_baseline_task, rootdir_minecraft_data, subject_id, extract_pkl, extract_csv, exclude, filter))
