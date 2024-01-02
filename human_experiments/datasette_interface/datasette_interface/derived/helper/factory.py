from sqlalchemy.orm import Session

from datasette_interface.derived.helper.eeg import EEGHelper
from datasette_interface.derived.helper.fnirs import FNIRSHelper
from datasette_interface.derived.helper.modality import ModalityHelper


def create_modality_helper(
    modality: str, group_session: str, station: str, db: Session
) -> ModalityHelper:
    """
    Creates a modality helper from a name.

    :param modality: name of the modality. One of eeg or fnirs.
    :param group_session: group session to process.
    :param station: station to process.
    :param db: active database session.
    """
    if modality == "eeg":
        return EEGHelper(group_session=group_session, station=station, db=db)

    if modality == "fnirs":
        return FNIRSHelper(group_session=group_session, station=station, db=db)

    raise ValueError(f"Invalid modality ({modality}).")
