def get_start_stop_time_from_xdf(data):
    """
    This function accepts two lists from which start and stop time is calculated. 
    The start & stop time will be common for all streams.
    In order to get the actual time you have to add time and a value which is under
    data['footer']['info']['clock_offsets'][0]['offset'][0] for the first timestamp and
    data['footer']['info']['clock_offsets'][0]['offset'][-1] for the last timestamp
    """
    time_start = data['footer']['info']['clock_offsets'][0]['offset'][0]['time']
    time_start = ''.join(map(str, time_start))
    time_start = float(time_start)

    value_start = data['footer']['info']['clock_offsets'][0]['offset'][0]['value']
    value_start = ''.join(map(str, value_start))
    value_start = float(value_start)

    time_start_streams = time_start+value_start

    end_start = data['footer']['info']['clock_offsets'][0]['offset'][-1]['time']
    end_start = ''.join(map(str, end_start))
    end_start = float(end_start)

    value_end = data['footer']['info']['clock_offsets'][0]['offset'][-1]['value']
    value_end = ''.join(map(str, value_end))
    value_end = float(value_end)

    time_end_streams = end_start + value_end

    return time_start_streams, time_end_streams
