import os
import pandas as pd
from time import ctime
from termcolor import colored 
from .baseline_tasks_timestamps import read_baseline_tasks_time
from .minecraft_timestamps import read_minecraft_time

def get_timestamps_from_dict(df, state, dict, column_name):
        df_temp = df
        rest_state_time_start, rest_state_time_stop  = dict['start_time'], dict['end_time']
        # print(rest_state_time_start, rest_state_time_stop)
        iloc_idx_start = df_temp['human_readable_time'].searchsorted(ctime(round(rest_state_time_start, 5))) 
        iloc_idx_end = df_temp['human_readable_time'].searchsorted(ctime(round(rest_state_time_stop, 5)))
        state_start = df_temp.index[iloc_idx_start]
        state_end = df_temp.index[iloc_idx_end]
        print(state_start, state_end)
        range_ = list(range(state_start, state_end))
        state = [state] * len(range_)
        state ={i:x for i,x in enumerate(state, state_start)}
        # print(df.index.map(state))
        # df_temp.insert(loc = (int(state_start), int(state_end)) , column = column_name, value= pd.Series(state, index=[state_start, state_end]))
        # df_temp[state_start:state_end, column_name] = state_end
        return state

def sync_timestamps_with_df(df, final_state, header):
    df[header] = df.index.map(final_state)
    return df

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
    print(colored('[INFO]', 'red', attrs=['bold']), 
        colored(stream_type, 'blue'), 
        colored('data written to CSV file', 'red', attrs=['bold']))

    #2. Gather human readable timestamp distribution
    df[header[1]] = time_distribution_human_readable
    print(colored('[INFO]', 'red', attrs=['bold']), 
        colored(stream_type, 'blue'), 
        colored('readable timestamp written to CSV file', 'red', attrs=['bold']))

    #3. Gather unix timestamp distribution
    df[header[0]] = time_distribution_unix
    print(colored('[INFO]', 'red', attrs=['bold']), 
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
        print(idx, dict.values())
        if 'rest_state' in dict.values():
            print(colored('[INFO]', 'red', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Rest state timestamps synced with CSV file', 'red', attrs=['bold']))
            state = 'rest_state'
            get_state_rest = get_timestamps_from_dict(df, state, dict, header[2])
        
        if 'finger_tapping' in dict.values():
            print(colored('[INFO]', 'red', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Finger tapping timestamps synced with CSV file', 'red', attrs=['bold']))
            state = 'finger_tapping'
            get_state_fingertap = get_timestamps_from_dict(df, state, dict, header[2])

        if 'affective_task_team' in dict.values():
            print(colored('[INFO]', 'red', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Affective team task timestamps synced with CSV file', 'red', attrs=['bold']))
            state = 'affective_task_team'
            get_state_affective_team = get_timestamps_from_dict(df, state, dict, header[2])

        if 'ping_pong_cooperative_0' in dict.values():
             print(colored('[INFO]', 'red', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Ping pong cooperative 0 task timestamps synced with CSV file', 'red', attrs=['bold']))           
             state = 'ping_pong_cooperative_0'
             get_state_pingpong_coop_0 = get_timestamps_from_dict(df, state, dict, header[2])

        if 'ping_pong_competetive_0' in dict.values():
             print(colored('[INFO]', 'red', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Ping pong competetive 0 task timestamps synced with CSV file', 'red', attrs=['bold']))           
             state = 'ping_pong_competetive_0'
             get_state_pingpong_comp_0 = get_timestamps_from_dict(df, state, dict, header[2])

        if 'ping_pong_competetive_1' in dict.values():
            print(colored('[INFO]', 'red', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Ping pong competetive 1 task timestamps synced with CSV file', 'red', attrs=['bold']))
            state = 'ping_pong_competetive_1'
            get_state_pingpong_comp_1 = get_timestamps_from_dict(df, state, dict, header[2])
        
        if 'hands_on_training' in dict.values():
            print(colored('[INFO]', 'red', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Minecraft hands on training timestamps synced with CSV file', 'red', attrs=['bold']))
            state = 'hands_on_training'
            mincraft_handson_training = get_timestamps_from_dict(df, state, dict, header[2])

        if 'saturn_a' in dict.values():
            print(colored('[INFO]', 'red', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Minecraft Saturn A timestamps synced with CSV file', 'red', attrs=['bold']))
            state = 'saturn_a'
            mincraft_saturn_a = get_timestamps_from_dict(df, state, dict, header[2])

        if 'saturn_b' in dict.values():
            print(colored('[INFO]', 'red', attrs=['bold']), 
                colored(stream_type, 'blue'), 
                colored('Minecraft Saturn B timestamps synced with CSV file', 'red', attrs=['bold']))
            state = 'saturn_b'
            mincraft_saturn_b = get_timestamps_from_dict(df, state, dict, header[2])

    final_state =  {**get_state_rest, **get_state_fingertap, 
                    **get_state_affective_team, **get_state_pingpong_coop_0, 
                    **get_state_pingpong_comp_0, **get_state_pingpong_comp_1, 
                    **mincraft_handson_training, **mincraft_saturn_a, 
                    **mincraft_saturn_b}
    # print(final_state)
    df = sync_timestamps_with_df(df, final_state, header[2])

        # if 'rest_state' in dict.values():
        #     print(colored('[INFO]', 'red', attrs=['bold']), 
        #         colored(stream_type, 'blue'), 
        #         colored('Rest state timestamps synced with CSV file', 'red', attrs=['bold']))
        #     state = 'rest_state'
        #     df.update(sync_with_df(df, state, dict, header[2]), overwrite=False)
        #     print(df[header[2]].unique())

        # if 'finger_tapping' in dict.values():
        #     print(colored('[INFO]', 'red', attrs=['bold']), 
        #         colored(stream_type, 'blue'), 
        #         colored('Finger tapping timestamps synced with CSV file', 'red', attrs=['bold']))
        #     state = 'finger_tapping'
        #     df.update(sync_with_df(df, state, dict, header[2]), overwrite=False)
        #     print(df[header[2]].unique())

        # if 'affective_task_team' in dict.values():
        #     print(colored('[INFO]', 'red', attrs=['bold']), 
        #         colored(stream_type, 'blue'), 
        #         colored('Affective team task timestamps synced with CSV file', 'red', attrs=['bold']))
        #     state = 'affective_task_team'
        #     df.update(sync_with_df(df, state, dict, header[2]), overwrite=False)
        #     print(df[header[2]].unique())

    df.to_csv(csv_file_name + ".csv", sep='\t', encoding='utf-8')
