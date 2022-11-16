import os 
import pandas as pd

def read_baseline_tasks_time(rootdir, subject_id): 
    start_stop_time = {} #Create a dictionary that would contain
    idx = 0 #index for the dictionary
    for x in sorted(os.listdir(rootdir)):
        if os.path.isdir(os.path.join(rootdir,x)): 
            if 'rest_state' in x:
                """
                It's a single CSV file with time common for 
                all subjects.
                """
                csvfiles = list(filter(lambda f: f.endswith('.csv'), os.listdir(os.path.join(rootdir,x))))
                
                for csvfile in csvfiles:
                    df = pd.read_csv(os.path.join(os.path.join(rootdir,x), csvfile), delimiter = ';')
                    start_stop_time[idx] = {'state':'rest_state', 'participant': None, 
                                    'start_time': df['time'].iloc[0], 
                                    'end_time':df['time'].iloc[-1]}
                    idx += 1
                    
            elif 'finger_tapping' in x:
                """
                It's a single CSV file with time common for 
                all subjects.
                """                
                csvfiles = list(filter(lambda f: f.endswith('.csv'), os.listdir(os.path.join(rootdir,x))))
                for csvfile in csvfiles:
                    df = pd.read_csv(os.path.join(os.path.join(rootdir,x), csvfile), delimiter = ';')
                    start_stop_time[idx] = {'state':'finger_tapping', 'participant': None, 
                                    'start_time': df['time'].iloc[0], 
                                    'end_time':df['time'].iloc[-1]}          
                    idx += 1         

            elif 'affective' in x:
                """
                There will be four csv files, from which three csv
                files contain image rating info for each subject. The fourth 
                csv file called 'team_*' would contain image rating info
                for all three paticipants as they work as team here. 
                """                  
                csvfiles = list(filter(lambda f: f.endswith('.csv'), os.listdir(os.path.join(rootdir,x))))
                cnt = 0
                for csvfile in sorted(csvfiles):
                    if 'team_' in csvfile: #if csv filename is team_*
                        df = pd.read_csv(os.path.join(os.path.join(rootdir,x), csvfile), delimiter = ';')
                        start_stop_time[idx] = {'state':'affective_task_team', 'participant': None, 
                                                'start_time': df['time'].iloc[0], 
                                                'end_time':df['time'].iloc[-1]}      
                        idx += 1
                    else:#if csv filename is individual_*
                        # mod_sub_id = []
                        for sid in sorted(subject_id):
                            if sid in csvfile:
                                sid = 'lion' if cnt == 0 else 'tiger' if cnt == 1 else 'leopard'
                                # mod_sub_id = mod_sub_id.append(sid)
                                df = pd.read_csv(os.path.join(os.path.join(rootdir,x), csvfile), delimiter = ';')
                                start_stop_time[idx] = {'state':'affective_task_individual', 'participant': sid, 
                                                        'start_time': df['time'].iloc[0], 
                                                        'end_time':df['time'].iloc[-1]}      
                                idx += 1
                                cnt += 1
                            
            elif 'ping_pong' in x:
                """
                Ping-pong task has 3 rounds:
                    Round 1: File name would be competetive_0_* 
                    where lion would play against tiger. Lion
                    and tiger would have the same time. 
                
                    Round 2: File name would be competetive_1_* 
                    where leopard would play against a experimenter. 
                    We will only consider leopard's time as the experimentor
                    isn't hooked up to physio devices. 
                
                    Round 3: File name would be cooperative_0* 
                    where lion, tiger and leopard would play against AI. 
                """
                csvfiles = list(filter(lambda f: f.endswith('.csv'), os.listdir(os.path.join(rootdir,x))))
                for csvfile in sorted(csvfiles):
                    if 'competitive_0' in csvfile:
                        df = pd.read_csv(os.path.join(os.path.join(rootdir,x), csvfile), delimiter = ';')
                        df = df.iloc[: , :-1]
                        
                        subject_id_pp = []
                        for sub_id in df.columns[idx:]: subject_id_pp.append(sub_id)
                        subject_id_pp = {x.removesuffix('_x').removesuffix('_y') for x in subject_id_pp}
                        #print(subject_id_pp)
                        num_sub = len(subject_id)
                        
                        for count, subj_id in enumerate(subject_id_pp):
                            for sid in subject_id:
                                if sid in subj_id:
                                    start_stop_time[idx] = {'state':'ping_pong_competetive_0', 'participant': sid, 
                                                            'start_time': df['time'].iloc[0], 
                                                            'end_time':df['time'].iloc[-1]}      
                                    idx += 1
                    elif 'competitive_1' in csvfile:
                        df = pd.read_csv(os.path.join(os.path.join(rootdir,x), csvfile), delimiter = ';')
                        df = df.iloc[: , :-1]
                        
                        subject_id_pp = []
                        for sub_id in df.columns[idx:]: subject_id_pp.append(sub_id)
                        subject_id_pp = {x.removesuffix('_x').removesuffix('_y') for x in subject_id_pp}
                        #print(subject_id_pp)
                        num_sub = len(subject_id)
                        
                        for count, subj_id in enumerate(subject_id_pp):
                            for sid in subject_id:
                                if sid in subj_id:
                                    start_stop_time[idx] = {'state':'ping_pong_competetive_1', 'participant': sid, 
                                                            'start_time': df['time'].iloc[0], 
                                                            'end_time':df['time'].iloc[-1]}      
                                    idx += 1

                    elif 'cooperative_0' in csvfile:
                        df = pd.read_csv(os.path.join(os.path.join(rootdir,x), csvfile), delimiter = ';')
                        df = df.iloc[: , :-1]
                        
                        subject_id_pp = []
                        for sub_id in df.columns[idx:]: subject_id_pp.append(sub_id)
                        subject_id_pp = {x.removesuffix('_x').removesuffix('_y') for x in subject_id_pp}
                        #print(subject_id_pp)
                        num_sub = len(subject_id)
                        
                        for count, subj_id in enumerate(subject_id_pp):
                            for sid in subject_id:
                                if sid in subj_id:
                                    start_stop_time[idx] = {'state':'ping_pong_cooperative_0', 'participant': sid, 
                                                            'start_time': df['time'].iloc[0], 
                                                            'end_time':df['time'].iloc[-1]}      
                                    idx += 1                                    
    return start_stop_time
