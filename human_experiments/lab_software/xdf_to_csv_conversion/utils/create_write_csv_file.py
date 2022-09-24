import os
import pandas as pd
from termcolor import colored 
from .baseline_tasks_timestamps import read_baseline_tasks_time
from .minecraft_timestamps import read_minecraft_time

def dataframe_to_csv(path, data, stream_type, time_distribution_human_readable, time_distribution_unix, 
                    rootdir_baseline_task, rootdir_minecraft_data, subject_id):
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
    print(colored('[Status]', 'red', attrs=['bold']), 
        colored(stream_type, 'blue'), 
        colored('data written to CSV file', 'red', attrs=['bold']))

    #2. Gather human readable timestamp distribution
    df[header[1]] = time_distribution_human_readable
    print(colored('[Status]', 'red', attrs=['bold']), 
        colored(stream_type, 'blue'), 
        colored('readable timestamp written to CSV file', 'red', attrs=['bold']))

    #3. Gather unix timestamp distribution
    df[header[0]] = time_distribution_unix
    print(colored('[Status]', 'red', attrs=['bold']), 
        colored(stream_type, 'blue'), 
        colored('unix timestamp written to CSV file', 'red', attrs=['bold']))    

    #4. Gather baseline task timestamp 
    """
    function read_rest_state_time() will return a 
    dictionary with structure {'state', 'participant', 
                                'start_time', 'end_time'}  
    """
    baseline_task_time = read_baseline_tasks_time(rootdir_baseline_task, subject_id)

    #4. Gather minecraft timestamp 
    """
    We will use the same dictionary rest_state_time
    and add minecraft timestamp. 
    """
    read_minecraft_time(baseline_task_time, rootdir_minecraft_data)
    df.to_csv(csv_file_name + ".csv", sep='\t', encoding='utf-8')
