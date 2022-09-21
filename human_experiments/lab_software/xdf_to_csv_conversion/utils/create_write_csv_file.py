import os
import csv
from unicodedata import name
import pandas as pd

#from baseline_tasks_timestamps import read_rest_state_time

def dataframe_to_csv(path, data, stream_type, time_distribution_human_readable, time_distribution_unix):
    """
    Read data from the XDF file, convert it into a dictionary, 
    then convert that to a pandas dataframe and save it as csv file. 
    """
    path = os.path.normpath(path + os.sep + os.pardir)
    if stream_type == 'NIRS':
        header = ['unix_time', 'human_readable_time', 'event_type','S1-D1_HbO', 'S1-D2_HbO', 'S2-D1_HbO', 
        'S2-D3_HbO', 'S3-D1_HbO', 'S3-D3_HbO', 'S3-D4_HbO', 'S4-D2_HbO', 'S4-D4_HbO', 'S4-D5_HbO', 'S5-D3_HbO', 
        'S5-D4_HbO', 'S5-D6_HbO', 'S6-D4_HbO', 'S6-D6_HbO', 'S6-D7_HbO', 'S7-D5_HbO', 'S7-D7_HbO', 'S8-D6_HbO', 
        'S8-D7_HbO', 'S1-D1_HbR', 'S1-D2_HbR', 'S2-D1_HbR', 'S2-D3_HbR', 'S3-D1_HbR', 'S3-D3_HbR', 'S3-D4_HbR', 
        'S4-D2_HbR', 'S4-D4_HbR', 'S4-D5_HbR', 'S5-D3_HbR', 'S5-D4_HbR', 'S5-D6_HbR', 'S6-D4_HbR', 'S6-D6_HbR', 
        'S6-D7_HbR', 'S7-D5_HbR', 'S7-D7_HbR', 'S8-D6_HbR', 'S8-D7_HbR']
        channel_list = header[3:]
        index = 41 #This points to index where HbO channel data starts

    data_path = path
    csv_file_name = data_path + '/' + stream_type
    df = pd.DataFrame(columns = header)
    
    csv_entry = {}
    for i in range(len(data)):
        csv_entry[i] = data[i][index:]
    
    #1. Gather all the channel data into the data frame
    df = pd.DataFrame.from_dict(csv_entry, columns=channel_list, orient='index')

    #2. Gather human readable timestamp distribution
    df[header[1]] = time_distribution_human_readable

    #3. Gather unix timestamp distribution
    df[header[0]] = time_distribution_unix
    
    df.to_csv(csv_file_name + ".csv", sep='\t', encoding='utf-8')