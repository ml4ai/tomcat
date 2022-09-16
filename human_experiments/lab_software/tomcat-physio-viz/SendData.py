"""Example program to demonstrate how to send a multi-channel time series to
LSL."""

import random
import time

from pylsl import StreamInfo, StreamOutlet

eeg = ('BioSemi', 'EEG', 34, 500, 'float32', 'myuid34234')
fNIRS = ('BioSemi', 'NIRS', 80, 10, 'float32', 'myuid34234')

name, content_type, n_channels, frequency, data_type, serial_number = fNIRS
nap_time = 1.0 / frequency

print(name, content_type, n_channels, frequency, data_type, serial_number, nap_time)

mysample = [0 for _ in range(n_channels)]
# exit()

# first create a new stream info (here we set the name to BioSemi,
# the content-type to EEG, 8 channels, 100 Hz, and float-valued data) The
# last value would be the serial number of the device or some other more or
# less locally unique identifier for the stream as far as available (you
# could also omit it but interrupted connections wouldn't auto-recover).
info = StreamInfo(name, content_type, n_channels, frequency, data_type, serial_number)

# next make an outlet
outlet = StreamOutlet(info)

print("now sending data...")
i = 0
while True:
    # make a new random 8-channel sample; this is converted into a
    # pylsl.vectorf (the data type that is expected by push_sample)
    for idx in range(n_channels):
        mysample[idx] = random.random()
    '''        
    mysample = [random.random(), random.random(), random.random(),
                random.random(), random.random(), random.random(),
                random.random(), random.random(), random.random(),
                random.random(), random.random(), random.random(),
                random.random(), random.random(), random.random(),
                random.random(), random.random(), random.random(),
                random.random(), random.random(), random.random(),
                random.random(), random.random(), random.random(),
                random.random(), random.random(), random.random(),
                random.random(), random.random(), random.random(),
                random.random(), random.random(), random.random(),
                random.random()]
    '''
    i += 1
    print(i)
    # now send it and wait for a bit
    outlet.push_sample(mysample)
    time.sleep(nap_time)
