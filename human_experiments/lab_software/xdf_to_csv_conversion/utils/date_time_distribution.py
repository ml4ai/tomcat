import pandas as pd
import datetime

def date_time_distribution(time_start_streams, time_end_streams, len_of_stream):
    start = datetime.datetime.fromtimestamp(time_start_streams).strftime("%Y-%m-%d %H:%M:%S.%f")
    ends = datetime.datetime.fromtimestamp(time_end_streams).strftime("%Y-%m-%d %H:%M:%S.%f")

    timestamps=pd.date_range(start=start,
                  end=ends,
                  periods=len_of_stream)
    
    return timestamps