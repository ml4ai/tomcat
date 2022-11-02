import os
from termcolor import colored

def check_xdf(rootdir):
    for root, dirs, files in os.walk(rootdir):
        for file in files:
            try:
                if file.endswith(".xdf"):
                    if os.stat(root+'/'+file).st_size!=0:
                        print(
                            colored('\n [Status] xdf file found at ', 'blue', attrs=['bold']), 
                            colored(os.path.join(root, file), 'cyan')
                            )
            except:
                print(
                    colored('[Error] xdf file is either empty or not present', 'red', attrs=['bold']),) 