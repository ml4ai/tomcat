from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Boolean
from sqlalchemy import Integer
from sqlalchemy import Text

from entity.base.base import Base


class DataValidity(Base):
    __tablename__ = "data_validity"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"), primary_key=True)
    participant_id: Mapped[int] = mapped_column("participant", Integer, ForeignKey("participant.id"), primary_key=True)
    station_id: Mapped[str] = mapped_column("station", Text, ForeignKey("station.id"), primary_key=True)
    task_id: Mapped[str] = mapped_column("task", Text, ForeignKey("task.id"), primary_key=True)
    modality_id: Mapped[str] = mapped_column("modality", Text, ForeignKey("modality.id"), primary_key=True)
    is_valid: Mapped[bool] = mapped_column(Boolean, primary_key=True)

    # No need to create objects (GroupSession etc.) at this point. Reassess that in the future.
    # e.g. group_session: Mapped["GroupSession"] = relationship(back_populates="data_validity")

    def __init__(self, group_session_id: str, participant_id: int, station_id: str, task_id: str, modality_id: str, is_valid: bool):
        super().__init__()

        self.group_session_id = group_session_id
        self.participant_id = participant_id
        self.station_id = station_id
        self.task_id = task_id
        self.modality_id = modality_id
        self.is_valid = is_valid
