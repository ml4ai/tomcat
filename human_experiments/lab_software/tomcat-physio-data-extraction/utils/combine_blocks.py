'''
New datapline has is broken up into blocks. This script combines the blocks into a single file.
'''

import numpy as np

def combine_blocks(blocks, block_1):
    # Create lists to hold the 'time_series' and 'time_stamps' for each device
    time_series_lion_0297, time_stamps_lion_0297 = [], []
    time_series_tiger_0239, time_stamps_tiger_0239 = [], []
    time_series_leopard_0171, time_stamps_leopard_0171 = [], []

    # Iterate over the blocks and combine the 'time_series' and 'time_stamps' data into the lists
    for block in blocks:
        if block['info']['name'][0] == 'lion_0297' and block['info']['type'][0] == 'NIRS':
            print('Im in', block['info']['name'][0])
            time_series_lion_0297 = np.concatenate((time_series_lion_0297, np.array(block['time_series'])))
            time_stamps_lion_0297 = np.concatenate((time_stamps_lion_0297, np.array(block['time_stamps'])))

        if block['info']['name'][0] == 'tiger_0239' and block['info']['type'][0] == 'NIRS':
            print('Im in', block['info']['name'][0])
            time_series_tiger_0239 = np.concatenate((time_series_tiger_0239, np.array(block['time_series'])))
            time_stamps_tiger_0239 = np.concatenate((time_stamps_tiger_0239, np.array(block['time_stamps'])))

        if block['info']['name'][0] == 'leopard_0171' and block['info']['type'][0] == 'NIRS':
            print('Im in', block['info']['name'][0])
            time_series_leopard_0171 = np.concatenate((time_series_leopard_0171, np.array(block['time_series'])))
            time_stamps_leopard_0171 = np.concatenate((time_stamps_leopard_0171, np.array(block['time_stamps'])))

    for i in range(0,len(block_1)):
        if block_1[i]['info']['type'][0] == 'NIRS' and block_1[i]['info']['name'][0] == 'lion_0297':      
            print(i)
            block_temp_lion = block_1[i]
            block_temp_lion['time_series'] = time_series_lion_0297
            block_temp_lion['time_stamps'] = time_stamps_lion_0297

        if block_1[i]['info']['type'][0] == 'NIRS' and block_1[i]['info']['name'][0] == 'tiger_0239':      
            print(i)
            block_temp_tiger = block_1[i]
            block_temp_tiger['time_series'] = time_series_tiger_0239
            block_temp_tiger['time_stamps'] = time_stamps_tiger_0239

        if block_1[i]['info']['type'][0] == 'NIRS' and block_1[i]['info']['name'][0] == 'leopard_0171':
            print(i)
            block_temp_leopard = block_1[i]
            block_temp_leopard['time_series'] = time_series_leopard_0171
            block_temp_leopard['time_stamps'] = time_stamps_leopard_0171 
    
    return block_temp_lion, block_temp_tiger, block_temp_leopard