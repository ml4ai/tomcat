import csv
 import sys 
 import os
 import argparse
 import pandas as pd
 from termcolor import colored
 from time import sleep, ctime
 from tasks.finger_tapping_task.config import TOTAL_TIME as total_time_finger_tapping 
 from tasks.rest_state.config import TOTAL_TIME as total_time_rest_state
 from tasks.affective_task.config import TOTAL_TIME as total_time_affective_task
 from tasks.ping_pong_task.config import TOTAL_TIME as total_time_ping_pong_task

 def check_subject_id(subject_id, sid):
     num_sub = len(subject_id)
     for count, subj_id in enumerate(subject_id):
         if subj_id in sid:
             if count == num_sub-1:
                 print(colored('\t Subect ID:','magenta'), subject_id)
         else:
             print(colored('[Error] Subject ID does not match','red'), u'\N{cross mark}')    

 def read_csv_column_name(df, sid, idx, file_name):
     subject_id = []
     if file_name == 'finger_tapping':
         for sub_id in df.columns[idx:]: subject_id.append(sub_id)
         check_subject_id(subject_id, sid)

     if file_name == 'ping_pong':
         for sub_id in df.columns[idx:]: subject_id.append(sub_id)
         subject_id = {x.removesuffix('_x').removesuffix('_y') for x in subject_id}
         check_subject_id(subject_id, sid)

 def read_muliple_csv_files(path, csvfiles, total_time, file_name, sid):
     for csvfile in csvfiles:
         #display which file its currently reading 
         print(colored('\n\t File:','magenta'), csvfile)

         try:
             df = pd.read_csv(os.path.join(path, csvfile), delimiter = ';')
         except:
             #check if the csv file is empty or not
             print(colored('[Error] CSV file is empty','red'), u'\N{cross mark}')
             break

         #display start and stop the task
         print(colored('\t Task started at:', 'magenta'), ctime(df['time'].iloc[0]))
         print(colored('\t Task ended at:', 'magenta'), ctime(df['time'].iloc[-1]))
         delta = int(float(df['time'].iloc[-1]) - float(df['time'].iloc[0]))

         if delta <= total_time + 3:
             #check if the time taken by this session is within the range of SECONDS_PER_SESSION in task config file 
             print(colored('\t Time taken:', 'magenta'), delta, 'Seconds')
         else:
             print(delta)
             print(colored('[Error] Timing for task is off by a large margin','red'), u'\N{cross mark}')

         #check if the subject IDs passed as an argument match the ones in the CSV files

         if file_name == 'affective':
             #Affective task, the csv file name contains subject ID
             if 'team_' in csvfile:
                 #the team affective task has the subject name as rows
                 df = df.dropna()
                 check_subject_id(set(df['subject_id'].values.tolist()), sid)
         elif file_name == 'ping_pong':
             if 'competitive_0' in csvfile:
                 df = df.iloc[: , :-1]
                 print(colored('\t Two participants competeting against eachother','magenta'))
                 read_csv_column_name(df, sid, -4, file_name)
             elif 'competitive_1' in csvfile:
                 df = df.iloc[: , :-1]
                 print(colored('\t Single participant competeting against Experimenter','magenta'))
                 read_csv_column_name(df, sid, -4, file_name)
             elif 'cooperative_0' in csvfile:
                 df = df.iloc[: , :-1]
                 print(colored('\t Three participant competeting against AI','magenta'))
                 read_csv_column_name(df, sid, -6, file_name)

         elif file_name == 'rest_state':
             #rest state doesnt have any subject information
             continue
         elif file_name == 'finger_tapping':
             #finger tapping, last three columns name have subject ID
             read_csv_column_name(df, sid, -3, file_name)

 def csvread(path, all_files, file_name, sid):
     #count csv files for ever task
     csvfiles = list(filter(lambda f: f.endswith('.csv'), all_files))

     if file_name == 'finger_tapping':
         if len(csvfiles) == 1:
             read_muliple_csv_files(path, csvfiles, total_time_finger_tapping, file_name, sid)
         else:
             print(colored('\n[Error] CSV file under finger tapping task is missing','red'), u'\N{cross mark}')

     elif file_name == 'rest_state':
         if len(csvfiles) == 1:
             read_muliple_csv_files(path, csvfiles, total_time_rest_state, file_name, sid)
         else:
             print(colored('\n[Error] CSV file under rest state task is missing','red'), u'\N{cross mark}')

     elif file_name == 'affective':
         if len(csvfiles) == 4:
             read_muliple_csv_files(path, csvfiles, total_time_affective_task, file_name, sid)
         else:
             print(colored('\n[Error] CSV file under affective task is missing','red'), u'\N{cross mark}')

     elif file_name == 'ping_pong':
         if len(csvfiles) == 3:
             read_muliple_csv_files(path, csvfiles, total_time_ping_pong_task, file_name, sid)
         else:
             print(colored('\n[Error] CSV file under ping pong task is missing','red'), u'\N{cross mark}')

 def fcount(rootdir, sid):
     #check if subdirectories for baseline task exist or not
     file_names = sorted(['finger_tapping', 'rest_state', 'affective', 'ping_pong'])
     count = 0
     for x in sorted(os.listdir(rootdir)):
         if os.path.isdir(os.path.join(rootdir,x)): 
             if file_names[count] in x:
                 sleep(0.25)
                 print(colored('\n[Status] Sub Directory:', 'red', attrs=['bold']), colored(os.path.join(rootdir,x), 'green'), u'\N{check mark}')
                 csvread(os.path.join(rootdir,x), os.listdir(os.path.join(rootdir,x)), file_names[count], sid)
             else:
                 print(colored('\n[Error] File does not exist','red'), u'\N{cross mark}')
             count += 1
     #print('Number of folders:', count)

 if __name__ == "__main__":
     parser = argparse.ArgumentParser(description='Baseline Task Data Validation')
     parser.add_argument("--p", required=True, help="Enter the Path to folder with baseline task data")
     parser.add_argument("--s", required=True, help="Enter the Subject ID", action='append')
     arg = parser.parse_args()
     rootdir = arg.p
     sid = arg.s
     print(colored('[Status] Root Directory:', 'red', attrs=['bold']), colored(rootdir, 'blue'))
     sys.exit(fcount(rootdir, sid))