from typing import Optional

from sqlalchemy import REAL, ForeignKey, Integer, Text, func
from sqlalchemy.orm import Mapped, Session, mapped_column

from datasette_interface.database.config import Base
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.entity.base.station import Station
from datasette_interface.database.entity.base.task import Task


class EEGRaw(Base):
    __tablename__ = "eeg_raw"

    group_session_id: Mapped[str] = mapped_column(
        "group_session", Text, ForeignKey(GroupSession.id), primary_key=True
    )
    station_id: Mapped[str] = mapped_column(
        "station", Text, ForeignKey(Station.id), primary_key=True
    )
    participant_id: Mapped[int] = mapped_column(
        "participant", Integer, ForeignKey(Participant.id), primary_key=True
    )
    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    task_id: Mapped[Optional[str]] = mapped_column("task", Text, ForeignKey(Task.id))
    timestamp_unix: Mapped[str] = mapped_column(Text)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    aff1h: Mapped[float] = mapped_column(REAL)
    f7: Mapped[float] = mapped_column(REAL)
    fc5: Mapped[float] = mapped_column(REAL)
    c3: Mapped[float] = mapped_column(REAL)
    t7: Mapped[float] = mapped_column(REAL)
    tp9: Mapped[float] = mapped_column(REAL)
    pz: Mapped[float] = mapped_column(REAL)
    p3: Mapped[float] = mapped_column(REAL)
    p7: Mapped[float] = mapped_column(REAL)
    o1: Mapped[float] = mapped_column(REAL)
    o2: Mapped[float] = mapped_column(REAL)
    p8: Mapped[float] = mapped_column(REAL)
    p4: Mapped[float] = mapped_column(REAL)
    tp10: Mapped[float] = mapped_column(REAL)
    cz: Mapped[float] = mapped_column(REAL)
    c4: Mapped[float] = mapped_column(REAL)
    t8: Mapped[float] = mapped_column(REAL)
    fc6: Mapped[float] = mapped_column(REAL)
    fcz: Mapped[float] = mapped_column(REAL)
    f8: Mapped[float] = mapped_column(REAL)
    aff2h: Mapped[float] = mapped_column(REAL)
    aux_gsr: Mapped[float] = mapped_column(REAL)
    aux_ekg: Mapped[float] = mapped_column(REAL)

    # Channels not recorded in v2 of the experimental design.
    aff5h: Mapped[Optional[float]] = mapped_column(REAL)
    fc1: Mapped[Optional[float]] = mapped_column(REAL)
    cp5: Mapped[Optional[float]] = mapped_column(REAL)
    cp1: Mapped[Optional[float]] = mapped_column(REAL)
    po9: Mapped[Optional[float]] = mapped_column(REAL)
    oz: Mapped[Optional[float]] = mapped_column(REAL)
    po10: Mapped[Optional[float]] = mapped_column(REAL)
    cp6: Mapped[Optional[float]] = mapped_column(REAL)
    cp2: Mapped[Optional[float]] = mapped_column(REAL)
    fc2: Mapped[Optional[float]] = mapped_column(REAL)
    aff6h: Mapped[Optional[float]] = mapped_column(REAL)

    @staticmethod
    def get_next_id(database_engine, group_session_id, station_id):
        with Session(database_engine) as session:
            max_id = (
                session.query(func.max(EEGRaw.id))
                .filter_by(group_session_id=group_session_id, station_id=station_id)
                .scalar()
            )
            return max_id + 1 if max_id is not None else 1
