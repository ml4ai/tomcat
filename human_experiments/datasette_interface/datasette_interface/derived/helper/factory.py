from datasette_interface.derived.helper.modality import ModalityHelper
from datasette_interface.derived.helper.eeg import EEGHelper
from datasette_interface.derived.helper.fnirs import FNIRSHelper


def create_modality_helper(modality: str, group_session: str, station: str) -> ModalityHelper:
    """
    Creates a modality helper from a name.

    modality: name of the modality. One of eeg or fnirs.
    group_session: group session to process.
    station: station to process.
    """
    if modality == "eeg":
        return EEGHelper(group_session=group_session, station=station)

    if modality == "fnirs":
        return FNIRSHelper(group_session=group_session, station=station)

    raise ValueError(f"Invalid modality ({modality}).")
