import os
from termcolor import colored

def check_pupil_recorder(rootdir):
    for root, dirs, files in os.walk(rootdir):
        for file in files:
            try:
                if file.endswith(".mp4"):
                    if os.stat(root+'/'+file).st_size!=0:
                        print(
                            colored('\n [Status] Mp4 file of pupil capture file found at ', 'blue', attrs=['bold']), 
                            colored(os.path.join(root, file), 'cyan')
                            )
            except:
                print(
                    colored('[Error]  Mp4 file of pupil capture file is either empty or not present', 'red', attrs=['bold']),) 