import warnings
warnings.filterwarnings('ignore')
import mne
import numpy as np

def filter_EEG(data):
    sfreq = 500
    
    '''
    You need to convert microvolts to volts by multiplying by 1e-6, and it applies a preamp gain adjustment by dividing by 50 and then halving the result. If we only want to perform the conversion from microvolts to volts, the operation simplifies to data *= 1e-6.
    '''

    #1. Get EKG data
    EKG_data = data['AUX_EKG'] * 1e-6 
    info_EKG = mne.create_info(['AUX_EKG'], sfreq, 'ecg')
    raw_EKG = mne.io.RawArray(np.array(EKG_data)[np.newaxis, :], info_EKG)

    #2. Get GSR data
    GSR_data = data['AUX_GSR'] * 1e-6
    info_GSR = mne.create_info(['AUX_GSR'], sfreq, 'misc')
    raw_GSR = mne.io.RawArray(np.array(GSR_data)[np.newaxis, :], info_GSR)

    #3. Get EEG data
    channels_used = [
                        "AFF1h", "F7", "FC5", "C3", "T7", "TP9", "Pz", "P3", "P7", "O1", "O2", "P8",
                        "P4", "TP10", "Cz", "C4", "T8", "FC6", "FCz", "F8", "AFF2h"
                    ]
    EEG_data = data[channels_used] * 1e-6
    info_EEG = mne.create_info(channels_used, sfreq, 'eeg')
    raw_EEG = mne.io.RawArray(np.array(EEG_data).T, info_EEG)
    EEG_montage = 'standard_1005' # Electrode position file
    raw_EEG.set_montage(EEG_montage)
    raw_EEG_filtered = raw_EEG.copy().notch_filter(freqs=60, filter_length='auto', trans_bandwidth=9, notch_widths=2)

    #4. Replace original data with filtered data
    data['AUX_EKG'] = raw_EKG.get_data()[0]
    data['AUX_GSR'] = raw_GSR.get_data()[0]
    data[channels_used] = raw_EEG_filtered.get_data().T

    return data
