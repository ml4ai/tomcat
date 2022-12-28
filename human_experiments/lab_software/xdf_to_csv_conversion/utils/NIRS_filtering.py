import numpy as np
import pandas as pd
from termcolor import colored 
from scipy.signal import butter, sosfilt, sosfreqz, sosfiltfilt, filtfilt, lfilter_zi 

def butter_bandpass(lowcut, highcut, fs, order=5):
    return butter(order, [lowcut, highcut], fs=fs, btype='band')

def butter_bandpass_filter(data, lowcut, highcut, fs, order=5):
    b, a = butter_bandpass(lowcut, highcut, fs, order=order)
    y = lfilter(b, a, data)
    return y

def check_cv(data, path, iloc_idx_start, iloc_idx_end):
    '''
    Calculate coeffecient of variance for fNIRS raw siganls, if cv is 
    greater than 7.5 then discard then log that channel as
    bad. 
    https://doi.org/10.3390/app12010316 refer section 2.4. Channel Exclusion Criterion.
    This function recives raw+HbO data but will use raw W1 to calculate
    the cv. 
    '''
    print(type(data), data.shape)

    channels = {'S1-D1', 'S1-D2', 'S2-D1', 'S2-D3', 'S3-D1', 'S3-D3', 
                'S3-D4', 'S4-D2', 'S4-D4', 'S4-D5', 'S5-D3', 'S5-D4', 
                'S5-D6', 'S6-D4', 'S6-D6', 'S6-D7', 'S7-D5', 'S7-D7', 
                'S8-D6', 'S8-D7'}

    print(colored('[INFO]', 'green', attrs=['bold']), 
        colored('calulating coeffecient of variance of NIRS', 'green', attrs=['bold']))    

    data = pd.DataFrame(data[:, 1:21])

    cv = lambda x: np.std(x, ddof=1) / np.mean(x) * 100
    coef_var = cv(data[iloc_idx_start:iloc_idx_end])

    print(coef_var)
    cv_vals = coef_var

    channel_good_or_bad = (coef_var<7.5).replace({True: 'good_channel', False: 'bad_channel'})

    df = pd.DataFrame(list(zip(channels,cv_vals,channel_good_or_bad)), columns=['Channels', 'coeff_of_var', 'status'])
    print(df)
    df.to_csv(path+'NIRS_channel_quality.csv', index=False)


def filter_NIRS(data):
    '''
    Use butterworth bandpass filter with highpass set at 0.2hz and
    lowpass set to 0.01hz. Also with a filter order of 3. 
    https://doi.org/10.3389%2Ffnhum.2018.00505, 
    https://doi.org/10.3390/a11050067, 
    https://doi.org/10.3389/fnhum.2019.00331
    '''

    fs = 10.2
    lowcut = 0.01
    highcut = 0.2

    fb, fa = butter_bandpass(lowcut, highcut, fs)
    zi = lfilter_zi(fb, fa)

    for i in range(40):
        data.iloc[:, i] = butter_bandpass_filter(data.iloc[:, i], lowcut, highcut, fs, order=3)
    
    return data