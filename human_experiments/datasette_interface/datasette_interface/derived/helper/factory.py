from sqlalchemy import Engine

from datasette_interface.derived.helper.eeg import EEGHelper
from datasette_interface.derived.helper.fnirs import FNIRSHelper
from datasette_interface.derived.helper.modality import ModalityHelper


def create_modality_helper(
        modality: str, group_session: str, station: str, db_engine: Engine
) -> ModalityHelper:
    """
    Creates a modality helper from a name.

    :param modality: name of the modality. One of eeg or fnirs.
    :param group_session: group session to process.
    :param station: station to process.
    :param db_engine: database engine.
    """
    if modality == "eeg":
        return EEGHelper(group_session=group_session, station=station, db_engine=db_engine)

    if modality == "fnirs":
        return FNIRSHelper(group_session=group_session, station=station, db_engine=db_engine)

    raise ValueError(f"Invalid modality ({modality}).")
