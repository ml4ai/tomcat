import os
from termcolor import colored

def check_xdf(rootdir):
    xdf_file_paths = []
    for root, dirs, files in os.walk(rootdir):
        for file in files:
            if file.endswith(".xdf"):
                xdf_file_paths.append(os.path.join(root, file))
                print(
                    colored('[Status] xdf file found at ', 'blue', attrs=['bold']), 
                    colored(os.path.join(root, file), 'cyan'))