import time
import math
import pandas as pd
from datetime import datetime
from termcolor import colored

def utc_time_ns(seconds):
    fractional, integer = math.modf(seconds)
    dt_object = datetime.utcfromtimestamp(integer)  # convert timestamp to datetime object in UTC time
    ns = round(fractional * 1e7)
    nanosec_str = f"{ns:07d}"  # represent nanoseconds as a string, padded to 9 digits
    microsec_str = nanosec_str[:7]  # extract the first 6 digits as microseconds
    microsec_float = float(f"0.{microsec_str}")  # convert microseconds string to float
    dt_object = dt_object.replace(microsecond=int(microsec_float * 1e6))  # add microseconds to datetime object

    return dt_object.strftime("%Y-%m-%d %H:%M:%S.") + nanosec_str

def create_time_distribution(data):
    # """
    # Using pandas create a distribution
    # """
    # starts = datetime.fromtimestamp(starts).strftime("%Y-%m-%d %H:%M:%S.%f")
    # ends = datetime.fromtimestamp(ends).strftime("%Y-%m-%d %H:%M:%S.%f")

    # time_distribution_human_readable = pd.date_range(
    #     start=starts, end=ends, periods=count
    # )

    # time_distribution_unix = {}
    # for i in range(len(time_distribution_human_readable)):
    #     # convert human readable back to unix time
    #     date_obj = datetime.timestamp(time_distribution_human_readable[i])
    #     time_distribution_unix[i] = "{0:.7f}".format(date_obj)

    # # time_distribution_unix is a dictionary, so convert it into a list
    # time_distribution_unix_list = []
    # for key, value in time_distribution_unix.items():
    #     temp = value
    #     time_distribution_unix_list.append(temp)
    time_distribution_unix_list = data['time_stamps']

    ctime_list = []

    for i in range(len(data['time_stamps'])):
        timestamp_s = float(data['time_stamps'][i])
        ctimestamp = utc_time_ns(timestamp_s)
        ctime_list.append(ctimestamp)

    time_distribution_human_readable = ctime_list

    return time_distribution_human_readable, time_distribution_unix_list


# python3.9 run_physio_data_extraction.py --p1 /Users/calebjonesshibu/Desktop/tom/data/exp_2023_02_03_10/ --p2 /Users/calebjonesshibu/Desktop/tom/data/exp_2023_02_03_10/baseline_tasks/ --p3 /Users/calebjonesshibu/Desktop/tom/data/exp_2023_02_03_10/minecraft/ --s 00096 --s 00101 --s 00104 --filter True --output_path /Users/calebjonesshibu/Desktop/tom/extraction