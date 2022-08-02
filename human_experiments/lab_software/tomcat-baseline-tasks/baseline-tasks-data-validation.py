import sys 
import os
import argparse
import pandas as pd
from termcolor import colored
from time import sleep, ctime
from tasks.finger_tapping_task.config_finger_tapping_task import TOTAL_TIME as total_time_finger_tapping 
from tasks.rest_state.config_rest_state import TOTAL_TIME as total_time_rest_state
from tasks.affective_task.config_affective_task import TOTAL_TIME as total_time_affective_task
from tasks.ping_pong_task.config_ping_pong_task import TOTAL_TIME as total_time_ping_pong_task

def read_muliple_csv_files(path, csvfiles, total_time):
    for csvfile in csvfiles:
        #display which file its currently reading 
        print(colored('\n\t File:','magenta'), csvfile)
        
        df = pd.read_csv(os.path.join(path, csvfile), delimiter = ';')
        check_time(df, total_time)
        yield df

def check_time(df ,total_time):
    #display start and stop the task
    print(colored('\t Task started at:', 'magenta'), ctime(df['time'].iloc[0]))
    print(colored('\t Task ended at:', 'magenta'), ctime(df['time'].iloc[-1]))
    delta = int(float(df['time'].iloc[-1]) - float(df['time'].iloc[0]))

    if delta <= total_time + 3:
        #check if the time taken by this session is within the range of SECONDS_PER_SESSION in task config file 
        print(colored('\t Time taken:', 'magenta'), delta, 'Seconds')
    else:
        print(colored('[Error] Timing for task is off by a large margin','red'), u'\N{cross mark}')
    return 

def check_subject_id(df, sid, file_name):
    subject_id = []
    if file_name == 'affective':
        print('')
    else:
        for sub_id in df.columns[-3:]: subject_id.append(sub_id)
        if sorted(subject_id) == sorted(sid):
            print(colored('\t Subect ID:','magenta'), subject_id)
        else:
            print(colored('[Error] Subject ID does not match','red'), u'\N{cross mark}')

def fcount(rootdir, sid):
    #check if subdirectories for baseline task exist or not
    file_names = ['finger_tapping', 'rest_state', 'affective', 'ping_pong']
    count = 0
    for x in os.listdir(rootdir):
        if os.path.isdir(os.path.join(rootdir,x)): 
            if file_names[count] in x:
                sleep(0.25)
                print(colored('\n[Status] Sub Directory:', 'red', attrs=['bold']), colored(os.path.join(rootdir,x), 'green'), u'\N{check mark}')
                csvread(os.path.join(rootdir,x), os.listdir(os.path.join(rootdir,x)), file_names[count], sid)
            else:
                print(colored('\n[Error] File does not exist','red'), u'\N{cross mark}')
            count += 1
    #print('Number of folders:', count)

def csvread(path, all_files, file_name, sid):
    #count csv files for ever task
    csvfiles = list(filter(lambda f: f.endswith('.csv'), all_files))

    if file_name == 'finger_tapping':
        if len(csvfiles) == 1:
            df = read_muliple_csv_files(path, csvfiles, total_time_finger_tapping)
            check_subject_id(df, sid, file_name)
        else:
            print(colored('\n[Error] CSV file under finger tapping task is missing','red'), u'\N{cross mark}')

    elif file_name == 'rest_state':
        if len(csvfiles) == 1:
            read_muliple_csv_files(path, csvfiles, total_time_rest_state)
        else:
            print(colored('\n[Error] CSV file under rest state task is missing','red'), u'\N{cross mark}')

    elif file_name == 'affective':
        if len(csvfiles) == 4:
            read_muliple_csv_files(path, csvfiles, total_time_affective_task)
        else:
            print(colored('\n[Error] CSV file under affective task is missing','red'), u'\N{cross mark}')

    elif file_name == 'ping_pong':
        if len(csvfiles) == 3:
            read_muliple_csv_files(path, csvfiles, total_time_ping_pong_task)
        else:
            print(colored('\n[Error] CSV file under ping pong task is missing','red'), u'\N{cross mark}')

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Baseline Task Data Validation')
    parser.add_argument("--p", required=True, help="Enter the Path to folder with baseline task data")
    parser.add_argument("--s", required=True, help="Enter the Subject ID", action='append')
    arg = parser.parse_args()
    rootdir = arg.p
    sid = arg.s
    print(colored('[Status] Root Directory:', 'red', attrs=['bold']), colored(rootdir, 'blue'))
    sys.exit(fcount(rootdir, sid))
