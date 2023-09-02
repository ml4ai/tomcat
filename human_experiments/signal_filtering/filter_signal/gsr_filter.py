import neurokit2 as nk
import numpy as np


def gsr_filter(signal: np.ndarray) -> np.ndarray:
    return np.array(nk.eda_clean(signal, sampling_rate=500, method="neurokit"))
