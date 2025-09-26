from sqlalchemy import Boolean, ForeignKey, Integer, Text
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.modality import Modality
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.entity.base.station import Station
from datasette_interface.database.entity.base.task import Task


class DataValidity(Base):
    __tablename__ = "data_validity"

    group_session_id: Mapped[str] = mapped_column(
        "group_session", Text, ForeignKey(GroupSession.id), primary_key=True
    )
    participant_id: Mapped[int] = mapped_column(
        "participant", Integer, ForeignKey(Participant.id), primary_key=True
    )
    station_id: Mapped[str] = mapped_column(
        "station", Text, ForeignKey(Station.id), primary_key=True
    )
    task_id: Mapped[str] = mapped_column(
        "task", Text, ForeignKey(Task.id), primary_key=True
    )
    modality_id: Mapped[str] = mapped_column(
        "modality", Text, ForeignKey(Modality.id), primary_key=True
    )
    is_valid: Mapped[bool] = mapped_column(Boolean, primary_key=True)
