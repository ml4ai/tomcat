from signal.modality.eeg import EEG
from signal.modality.ekg import EKG
from signal.modality.fnirs import FNIRS
from signal.modality.gaze import Gaze

MODALITIES = {
    "eeg": EEG(),
    "ekg": EKG(),
    "fnirs": FNIRS(),
    "gaze": Gaze()
}
