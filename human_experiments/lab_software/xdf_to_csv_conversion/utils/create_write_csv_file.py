import os
import csv
import pandas as pd
#from baseline_tasks_timestamps import read_rest_state_time

def create_csv_file(path, stream_type):
    path = os.path.normpath(path + os.sep + os.pardir)
    if stream_type == 'NIRS':
        header = ['unix_time', 'human_readable_time', 'event_type','S1-D1_HbO', 'S1-D2_HbO', 'S2-D1_HbO', 
        'S2-D3_HbO', 'S3-D1_HbO', 'S3-D3_HbO', 'S3-D4_HbO', 'S4-D2_HbO', 'S4-D4_HbO', 'S4-D5_HbO', 'S5-D3_HbO', 
        'S5-D4_HbO', 'S5-D6_HbO', 'S6-D4_HbO', 'S6-D6_HbO', 'S6-D7_HbO', 'S7-D5_HbO', 'S7-D7_HbO', 'S8-D6_HbO', 
        'S8-D7_HbO', 'S1-D1_HbR', 'S1-D2_HbR', 'S2-D1_HbR', 'S2-D3_HbR', 'S3-D1_HbR', 'S3-D3_HbR', 'S3-D4_HbR', 
        'S4-D2_HbR', 'S4-D4_HbR', 'S4-D5_HbR', 'S5-D3_HbR', 'S5-D4_HbR', 'S5-D6_HbR', 'S6-D4_HbR', 'S6-D6_HbR', 
        'S6-D7_HbR', 'S7-D5_HbR', 'S7-D7_HbR', 'S8-D6_HbR', 'S8-D7_HbR']

    data_path = path
    csv_file_name = data_path + '/' + stream_type
    csv_file = open(csv_file_name + ".csv", 'w', newline='')
    csv_writer = csv.DictWriter(csv_file, delimiter=';', fieldnames = header)
    csv_writer.writeheader()

    return (csv_file_name + ".csv", csv_writer)

def write_to_csv_file(path, data, csv_writer):
    channel_list = ['S1-D1_HbO', 'S1-D2_HbO', 'S2-D1_HbO', 'S2-D3_HbO', 'S3-D1_HbO', 'S3-D3_HbO', 'S3-D4_HbO', 
    'S4-D2_HbO', 'S4-D4_HbO', 'S4-D5_HbO', 'S5-D3_HbO', 'S5-D4_HbO', 'S5-D6_HbO', 'S6-D4_HbO', 'S6-D6_HbO', 
    'S6-D7_HbO', 'S7-D5_HbO', 'S7-D7_HbO', 'S8-D6_HbO', 'S8-D7_HbO', 'S1-D1_HbR', 'S1-D2_HbR', 'S2-D1_HbR', 
    'S2-D3_HbR', 'S3-D1_HbR', 'S3-D3_HbR', 'S3-D4_HbR', 'S4-D2_HbR', 'S4-D4_HbR', 'S4-D5_HbR', 'S5-D3_HbR', 
    'S5-D4_HbR', 'S5-D6_HbR', 'S6-D4_HbR', 'S6-D6_HbR', 'S6-D7_HbR', 'S7-D5_HbR', 'S7-D7_HbR', 'S8-D6_HbR', 
    'S8-D7_HbR']
    #df = pd.read_csv(path)
    for i in range(len(data)):
        #print(len(data[i][1:]), len(channel_list))
        #df = df.append(pd.DataFrame(data[i][41:], columns= channel_list), ignore_index=True)
        for j in range(len(channel_list)):
            csv_entry = {channel_list[j] : data[i][j + 41]}
            print(i, j)
            csv_writer.writerow(csv_entry)