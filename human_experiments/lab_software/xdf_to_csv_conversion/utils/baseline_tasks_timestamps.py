import os 
from termcolor import colored 
from time import ctime
import pandas as pd

def read_rest_state_time(rootdir): 
    for x in sorted(os.listdir(rootdir)):
        if os.path.isdir(os.path.join(rootdir,x)): 
            if 'rest_state' in x:
                csvfiles = list(filter(lambda f: f.endswith('.csv'), os.listdir(os.path.join(rootdir,x))))
                print(csvfiles)
                for csvfile in csvfiles:
                    print(os.path.join(os.path.join(rootdir,x), csvfile))
                    df = pd.read_csv(os.path.join(os.path.join(rootdir,x), csvfile), delimiter = ';')
                print(ctime(df['time'].iloc[0]), ctime(df['time'].iloc[-1]))

# read_rest_state_time('/Users/calebjonesshibu/Desktop/tom/pilot/exp_2022_09_09_12/baseline_tasks')