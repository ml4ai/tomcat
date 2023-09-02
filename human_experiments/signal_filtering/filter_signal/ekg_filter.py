import neurokit2 as nk
import numpy as np


def ekg_filter(signal: np.ndarray) -> np.ndarray:
    return np.array(nk.ecg_clean(signal, sampling_rate=500, method="biosppy"))
