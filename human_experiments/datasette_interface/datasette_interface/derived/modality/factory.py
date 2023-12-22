from data_pre_processing.signal.entity.eeg import EEG
from data_pre_processing.signal.entity.ekg import EKG
from data_pre_processing.signal.entity.gsr import GSR
from data_pre_processing.signal.entity.fnirs import FNIRS
from data_pre_processing.signal.entity.gaze import Gaze


def create_modality(modality: str):
    if modality == "eeg":
        return EEG()

    if modality == "fnirs":
        return FNIRS()

    raise ValueError(f"Invalid modality ({modality}).")
