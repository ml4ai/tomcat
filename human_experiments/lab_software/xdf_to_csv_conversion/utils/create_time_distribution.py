import pandas as pd
from datetime import datetime

def create_time_distribution(starts, ends, count):
    """
    Using pandas create a distribution 
    """
    starts = datetime.fromtimestamp(starts).strftime("%Y-%m-%d %H:%M:%S.%f")
    ends = datetime.fromtimestamp(ends).strftime("%Y-%m-%d %H:%M:%S.%f")
    
    time_distribution = pd.date_range(start=starts,
                        end=ends,
                        periods=count)

    return time_distribution