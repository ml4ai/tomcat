import pandas as pd
from datetime import datetime
from termcolor import colored

def create_time_distribution(starts, ends, count):
    """
    Using pandas create a distribution 
    """
    starts = datetime.fromtimestamp(starts).strftime("%Y-%m-%d %H:%M:%S.%f")
    ends = datetime.fromtimestamp(ends).strftime("%Y-%m-%d %H:%M:%S.%f")
    
    time_distribution_human_readable = pd.date_range(start=starts,
                        end=ends,
                        periods=count)

    time_distribution_unix = {}
    for i in range(len(time_distribution_human_readable)):
        #convert human readable back to unix time
        date_obj = datetime.timestamp(time_distribution_human_readable[i])
        time_distribution_unix[i] = "{0:.7f}".format(date_obj)
    
    #time_distribution_unix is a dictionary, so convert it into a list
    time_distribution_unix_list = []
    for key, value in time_distribution_unix.items():
        temp = value
        time_distribution_unix_list.append(temp)

    return time_distribution_human_readable, time_distribution_unix_list