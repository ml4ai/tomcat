import os
import sys
import pyxdf
import argparse
from termcolor import colored
from utils import get_start_stop_time_from_xdf, dataframe_to_csv, create_time_distribution

def read_xdf(xdf_file_paths):
    """
    Read the XDF files.
    """
    for path in xdf_file_paths:
        data, header = pyxdf.load_xdf(path)
        for i in range(0,len(data)):
            if data[i]['info']['type'] == ['NIRS']:
                print(
                colored('[Status] Reading ', 'red', attrs=['bold']), 
                colored(data[i]['info']['type'], 'blue'))
                time_start_streams_nirs, time_end_streams_nirs = get_start_stop_time_from_xdf(data[i]) #get the unix time
                timestamp_distribution = create_time_distribution(time_start_streams_nirs, time_end_streams_nirs, len(data[i]['time_series']))
                dataframe_to_csv(path, data[i]['time_series'], 'NIRS', timestamp_distribution)

            elif data[i]['info']['type'] == ['Markers']:
                #We don't have physical marker for our physio data
                print(
                colored('[Status] Ignoring ', 'red', attrs=['bold']), 
                colored(data[i]['info']['type'], 'blue'))                

            elif data[i]['info']['type'] == ['EEG']:
                print(
                colored('[Status] Reading ', 'red', attrs=['bold']), 
                colored(data[i]['info']['type'], 'blue'))
                #create_csv_file(path, 'EEG')
                time_start_streams_eeg, time_end_streams_eeg = get_start_stop_time_from_xdf(data[i]) #get the unix time
                #dataframe_to_csv(path, data[i]['time_series'], 'EEG')

            elif data[i]['info']['type'] == ['Gaze']:
                print(
                colored('[Status] Reading ', 'red', attrs=['bold']), 
                colored(data[i]['info']['type'], 'blue'))
                #create_csv_file(path, 'Gaze')
                time_start_streams_gaze, time_end_streams_gaze = get_start_stop_time_from_xdf(data[i]) #get the unix time
                # dataframe_to_csv(path, data[i]['time_series'], 'Gaze')
            
            elif data[i]['info']['type'] == ['Accelerometer']:
                print(
                colored('[Status] Reading ', 'red', attrs=['bold']), 
                colored(data[i]['info']['type'], 'blue')) 
                #create_csv_file(path, 'Accelerometer')
                time_start_streams_accel, time_end_streams_accel = get_start_stop_time_from_xdf(data[i]) #get the unix time

def look_for_XDF_files(rootdir):
    """
    Walk through root directory, looking for the xdf files. 
    """
    xdf_file_paths = []
    for root, dirs, files in os.walk(rootdir):
        for file in files:
            if file.endswith(".xdf"):
                xdf_file_paths.append(os.path.join(root, file))
                print(
                    colored('[Status] xdf file found at ', 'red', attrs=['bold']), 
                    colored(os.path.join(root, file), 'blue'))
    
    read_xdf(xdf_file_paths) #1. read all the XDF files 

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

    arg = parser.parse_args()
    rootdir_xdf = arg.p1
    rootdir_baseline_task = arg.p2
    print(colored('[Status] Root Directory:', 'red', attrs=['bold']), colored(rootdir_xdf, 'blue'))
    sys.exit(look_for_XDF_files(rootdir_xdf))
