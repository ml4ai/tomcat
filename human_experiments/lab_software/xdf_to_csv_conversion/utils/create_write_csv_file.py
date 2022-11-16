import os
import pandas as pd
from time import ctime
from termcolor import colored 
from .baseline_tasks_timestamps import read_baseline_tasks_time
from .minecraft_timestamps import read_minecraft_time

def get_timestamps_from_dict(df, state, dict, column_name):
        df_temp = df
        rest_state_time_start, rest_state_time_stop  = dict['start_time'], dict['end_time']
        iloc_idx_start = df_temp['human_readable_time'].searchsorted(ctime(round(rest_state_time_start, 5))) 
        iloc_idx_end = df_temp['human_readable_time'].searchsorted(ctime(round(rest_state_time_stop, 5)))
        state_start = df_temp.index[iloc_idx_start]
        state_end = df_temp.index[iloc_idx_end]
        range_ = list(range(state_start, state_end))
        state = [state] * len(range_)
        state ={i:x for i,x in enumerate(state, state_start)}
        return state

def sync_timestamps_with_df(df, final_state, header, df_remove_before, df_remove_after):
    '''
    Map the states based index with the dataframe 
    and return dataframe from start of rest state to 
    end of saturn b minecraft mission. 
    '''
    df[header] = df.index.map(final_state)
    return df.loc[df_remove_before:df_remove_after]

def dataframe_to_csv(path, data, stream_type, time_distribution_human_readable, time_distribution_unix, 
                    rootdir_baseline_task, rootdir_minecraft_data, subject_id, extract_pkl, extract_csv):
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
        index = 41 #This points to index where HbO channel data starts on the XDF file

    if stream_type == 'EEG':
        header = ['unix_time', 'human_readable_time', 'event_type', 'AFF1h', 'AFF5h', 'F7', 'FC5', 'FC1', 'C3', 
        'T7', 'TP9', 'CP5', 'CP1', 'Pz', 'P3', 'P7', 'PO9', 'O1', 'Oz', 'O2', 'PO10', 'P8', 'P4', 'TP10', 'CP6', 
        'CP2', 'Cz', 'C4', 'T8', 'FC6', 'FC2', 'FCz', 'F8', 'AFF6h', 'AFF2h', 'AUX_GSR', 'AUX_EKG']
        channel_list = header[3:]
        index = 0 #EEG starts at the 0th index on the XDF file

    if stream_type == 'Gaze':
        header = ['unix_time', 'human_readable_time', 'event_type', 'confidence', 'norm_pos_x', 'norm_pos_y',
        'gaze_point_3d_x', 'gaze_point_3d_y', 'gaze_point_3d_z', 'eye_center0_3d_x', 'eye_center0_3d_y',
        'eye_center0_3d_z', 'eye_center1_3d_x', 'eye_center1_3d_y', 'eye_center1_3d_z', 'gaze_normal0_x',
        'gaze_normal0_y', 'gaze_normal0_z', 'gaze_normal1_x', 'gaze_normal1_y', 'gaze_normal1_z', 'diameter0_2d',
        'diameter1_2d', 'diameter0_3d', 'diameter1_3d']
        channel_list = header[3:]
        index = 0 #Gaze starts at the 0th index on the XDF file

    data_path = path
    csv_file_name = data_path + '/' + stream_type
    df = pd.DataFrame(columns = header)
    
    csv_entry = {}
    for i in range(len(data)):
        csv_entry[i] = data[i][index:]
    
    #1. Gather all the channel data into the data frame
    df = pd.DataFrame.from_dict(csv_entry, columns=channel_list, orient='index')
    print(colored('[INFO]', 'green', attrs=['bold']), 
        colored(stream_type, 'blue'), 
        colored('data written to CSV file', 'green', attrs=['bold']))

    #2. Gather human readable timestamp distribution
    df[header[1]] = time_distribution_human_readable
    print(colored('[INFO]', 'green', attrs=['bold']), 
        colored(stream_type, 'blue'), 
        colored('readable timestamp written to CSV file', 'green', attrs=['bold']))

    #3. Gather unix timestamp distribution
    df[header[0]] = time_distribution_unix
    print(colored('[INFO]', 'green', attrs=['bold']), 
        colored(stream_type, 'blue'), 
        colored('unix timestamp written to CSV file', 'green', attrs=['bold']))    

    #4. Gather baseline task timestamp 
    """
    function read_rest_state_time() will return a 
    dictionary with structure {'state', 'participant', 
                                'start_time', 'end_time'}  
    """
    baseline_task_time = read_baseline_tasks_time(rootdir_baseline_task, subject_id)

    #4. Gather minecraft timestamp 
    """
    We will use the same dictionary baseline_task_time
    and add minecraft timestamp. 
    """
    all_task_time = read_minecraft_time(baseline_task_time, rootdir_minecraft_data)

    #5. Sync rest state timestamp with xdf timestamp
    """
    We find the closest timestamp to *_state_time_start, 
    *_state_time_stop from our dataframe. Then insert
    the state value to df
    """
    for idx, dict in all_task_time.items():
        if 'rest_state' in dict.values():
            print(colored('[INFO]', 'green', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Rest state timestamps synced with CSV file', 'green', attrs=['bold']))
            state = 'rest_state'
            get_state_rest = get_timestamps_from_dict(df, state, dict, header[2])
        
        if 'finger_tapping' in dict.values():
            print(colored('[INFO]', 'green', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Finger tapping timestamps synced with CSV file', 'green', attrs=['bold']))
            state = 'finger_tapping'
            get_state_fingertap = get_timestamps_from_dict(df, state, dict, header[2])

        if 'affective_task_individual' in dict.values() and any((str(iMac) in path) for iMac in dict.values()):
            """
            As different participants
            finish the task at different time, so based 
            based on the sid we save it to that particular
            folder. eg: if sid is lion it would go to lions
            folder. 
            """
            print(colored('[INFO]', 'green', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Affective individual task timestamps synced with CSV file', 'green', attrs=['bold']))
            state = 'affective_task_individual'
            affective_task_individual = get_timestamps_from_dict(df, state, dict, header[2])

        if 'affective_task_team' in dict.values():
            print(colored('[INFO]', 'green', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Affective team task timestamps synced with CSV file', 'green', attrs=['bold']))
            state = 'affective_task_team'
            get_state_affective_team = get_timestamps_from_dict(df, state, dict, header[2])

        if 'ping_pong_cooperative_0' in dict.values():
            print(colored('[INFO]', 'green', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Ping pong cooperative 0 task timestamps synced with CSV file', 'green', attrs=['bold']))           
            state = 'ping_pong_cooperative_0'
            get_state_pingpong_coop_0 = get_timestamps_from_dict(df, state, dict, header[2])

        if 'ping_pong_competetive_0' in dict.values():
            print(colored('[INFO]', 'green', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Ping pong competetive 0 task timestamps synced with CSV file', 'green', attrs=['bold']))           
            state = 'ping_pong_competetive_0'
            get_state_pingpong_comp_0 = get_timestamps_from_dict(df, state, dict, header[2])

        if 'ping_pong_competetive_1' in dict.values():
            print(colored('[INFO]', 'green', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Ping pong competetive 1 task timestamps synced with CSV file', 'green', attrs=['bold']))
            state = 'ping_pong_competetive_1'
            get_state_pingpong_comp_1 = get_timestamps_from_dict(df, state, dict, header[2])
        
        if 'hands_on_training' in dict.values():
            print(colored('[INFO]', 'green', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Minecraft hands on training timestamps synced with CSV file', 'green', attrs=['bold']))
            state = 'hands_on_training'
            mincraft_handson_training = get_timestamps_from_dict(df, state, dict, header[2])

        if 'saturn_a' in dict.values():
            print(colored('[INFO]', 'green', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Minecraft Saturn A timestamps synced with CSV file', 'green', attrs=['bold']))
            state = 'saturn_a'
            mincraft_saturn_a = get_timestamps_from_dict(df, state, dict, header[2])

        if 'saturn_b' in dict.values():
            print(colored('[INFO]', 'green', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Minecraft Saturn B timestamps synced with CSV file', 'green', attrs=['bold']))
            state = 'saturn_b'
            mincraft_saturn_b = get_timestamps_from_dict(df, state, dict, header[2])

    final_state =  {**get_state_rest, **get_state_fingertap, **affective_task_individual, 
                        **get_state_affective_team, **get_state_pingpong_coop_0, 
                        **get_state_pingpong_comp_0, **get_state_pingpong_comp_1, 
                        **mincraft_handson_training, **mincraft_saturn_a, 
                        **mincraft_saturn_b}
        
    df_remove_before = list(get_state_rest.keys())[0]
    df_remove_after = list(mincraft_saturn_b.keys())[-1]
    df_final = sync_timestamps_with_df(df, final_state, header[2], df_remove_before, df_remove_after)

    if extract_csv == True:
        df_final.to_csv(csv_file_name + ".csv", sep='\t', encoding='utf-8')
        print(colored('[INFO]', 'green', attrs=['bold']), 
                    colored('Sucessfully generated csv file at', 'green', attrs=['bold']), colored(csv_file_name + ".csv", 'blue'))
    if extract_pkl == True:
        df_final.to_pickle(csv_file_name + ".pkl")
        print(colored('[INFO]', 'green', attrs=['bold']), 
                    colored('Sucessfully generated pickle file at', 'green', attrs=['bold']), colored(csv_file_name + ".pkl", 'blue'))
        