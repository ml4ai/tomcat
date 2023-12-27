from typing import Optional

from sqlalchemy import REAL, ForeignKey, Integer, Text, func
from sqlalchemy.orm import Mapped, Session, mapped_column

from datasette_interface.database.config import Base
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.entity.base.station import Station
from datasette_interface.database.entity.base.task import Task


class FNIRSRaw(Base):
    __tablename__ = "fnirs_raw"

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
    s1_d1_hbo: Mapped[float] = mapped_column(REAL)
    s1_d2_hbo: Mapped[float] = mapped_column(REAL)
    s2_d1_hbo: Mapped[float] = mapped_column(REAL)
    s2_d3_hbo: Mapped[float] = mapped_column(REAL)
    s3_d1_hbo: Mapped[float] = mapped_column(REAL)
    s3_d3_hbo: Mapped[float] = mapped_column(REAL)
    s3_d4_hbo: Mapped[float] = mapped_column(REAL)
    s4_d2_hbo: Mapped[float] = mapped_column(REAL)
    s4_d4_hbo: Mapped[float] = mapped_column(REAL)
    s4_d5_hbo: Mapped[float] = mapped_column(REAL)
    s5_d3_hbo: Mapped[float] = mapped_column(REAL)
    s5_d4_hbo: Mapped[float] = mapped_column(REAL)
    s5_d6_hbo: Mapped[float] = mapped_column(REAL)
    s6_d4_hbo: Mapped[float] = mapped_column(REAL)
    s6_d6_hbo: Mapped[float] = mapped_column(REAL)
    s6_d7_hbo: Mapped[float] = mapped_column(REAL)
    s7_d5_hbo: Mapped[float] = mapped_column(REAL)
    s7_d7_hbo: Mapped[float] = mapped_column(REAL)
    s8_d6_hbo: Mapped[float] = mapped_column(REAL)
    s8_d7_hbo: Mapped[float] = mapped_column(REAL)
    s1_d1_hbr: Mapped[float] = mapped_column(REAL)
    s1_d2_hbr: Mapped[float] = mapped_column(REAL)
    s2_d1_hbr: Mapped[float] = mapped_column(REAL)
    s2_d3_hbr: Mapped[float] = mapped_column(REAL)
    s3_d1_hbr: Mapped[float] = mapped_column(REAL)
    s3_d3_hbr: Mapped[float] = mapped_column(REAL)
    s3_d4_hbr: Mapped[float] = mapped_column(REAL)
    s4_d2_hbr: Mapped[float] = mapped_column(REAL)
    s4_d4_hbr: Mapped[float] = mapped_column(REAL)
    s4_d5_hbr: Mapped[float] = mapped_column(REAL)
    s5_d3_hbr: Mapped[float] = mapped_column(REAL)
    s5_d4_hbr: Mapped[float] = mapped_column(REAL)
    s5_d6_hbr: Mapped[float] = mapped_column(REAL)
    s6_d4_hbr: Mapped[float] = mapped_column(REAL)
    s6_d6_hbr: Mapped[float] = mapped_column(REAL)
    s6_d7_hbr: Mapped[float] = mapped_column(REAL)
    s7_d5_hbr: Mapped[float] = mapped_column(REAL)
    s7_d7_hbr: Mapped[float] = mapped_column(REAL)
    s8_d6_hbr: Mapped[float] = mapped_column(REAL)
    s8_d7_hbr: Mapped[float] = mapped_column(REAL)

    @staticmethod
    def get_next_id(database_engine, group_session_id, station_id):
        with Session(database_engine) as session:
            max_id = (
                session.query(func.max(FNIRSRaw.id))
                .filter_by(group_session_id=group_session_id, station_id=station_id)
                .scalar()
            )
            return max_id + 1 if max_id is not None else 1
