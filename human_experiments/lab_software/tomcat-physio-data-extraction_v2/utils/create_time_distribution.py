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
    time_distribution_unix_list = data['time_stamps']

    ctime_list = []

    for i in range(len(data['time_stamps'])):
        timestamp_s = float(data['time_stamps'][i])
        ctimestamp = utc_time_ns(timestamp_s)
        ctime_list.append(ctimestamp)

    time_distribution_human_readable = ctime_list

    return time_distribution_human_readable, time_distribution_unix_list
