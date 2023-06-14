def get_start_stop_time_from_xdf(data):
    """
    This function accepts two lists from which start and stop time is calculated.
    The start & stop time will be common for all streams.
    In order to get the actual time you have to add time and a value which is under
    data['footer']['info']['clock_offsets'][0]['offset'][0] for the first timestamp and
    data['footer']['info']['clock_offsets'][0]['offset'][-1] for the last timestamp
    """
    time_start_streams = data["time_stamps"][0]
    time_end_streams = data["time_stamps"][-1]

    return time_start_streams, time_end_streams
