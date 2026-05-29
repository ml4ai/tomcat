from pylsl import StreamInlet, resolve_stream

streams = resolve_stream()
# device_index = {'tiger_0239':0, 'lion_0297':1, 'leopard_0171':1}

# device_name = 'tiger_0239'
j = 0 
for i in range(len(streams)):
    if streams[i].type() == 'EEG':
        print(streams[i].name(), len(streams[i].type()))
    # j += 1
    # if device_index.keys[j] == streams[i].name():
    #     print(streams[i].name(), len(streams[i].type()))
    #     j += 1

# device_id = 0

# device_index = {'tiger_0239':0, 'lion_0297':1, 'leopard_0171':1}
# j = 0

# for i in range(len(streams)):
#     if streams[i].type() == 'NIRS' or streams[i].type() == 'NIRS':
#         print('Hi')
#         if device_index.keys[j] == streams[i].name():
#             print(streams[i].name(), len(streams[i].type()))
#             j += 1
