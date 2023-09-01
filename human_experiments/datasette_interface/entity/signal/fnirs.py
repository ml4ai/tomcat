from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import REAL
from sqlalchemy import Text
from sqlalchemy import func
from sqlalchemy.orm import Mapped
from sqlalchemy.orm import Session
from sqlalchemy.orm import mapped_column

from entity.base.base import Base


class FNIRSRaw(Base):
    __tablename__ = "fnirs_raw"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"),
                                                  primary_key=True)
    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    station_id: Mapped[str] = mapped_column("station", Text, ForeignKey("station.id"), primary_key=True)
    participant_id: Mapped[int] = mapped_column("participant", Integer, ForeignKey("participant.id"), primary_key=True)
    task_id: Mapped[Optional[str]] = mapped_column("task", Text, ForeignKey("task.id"))
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

    def __init__(self, group_session_id: str, id: int, task_id: Optional[str], station_id: str, participant_id: int,
                 timestamp_unix: str, timestamp_iso8601: str, s1_d1_hbo: float, s1_d2_hbo: float, s2_d1_hbo: float,
                 s2_d3_hbo: float, s3_d1_hbo: float, s3_d3_hbo: float, s3_d4_hbo: float, s4_d2_hbo: float,
                 s4_d4_hbo: float, s4_d5_hbo: float, s5_d3_hbo: float, s5_d4_hbo: float, s5_d6_hbo: float,
                 s6_d4_hbo: float, s6_d6_hbo: float, s6_d7_hbo: float, s7_d5_hbo: float, s7_d7_hbo: float,
                 s8_d6_hbo: float, s8_d7_hbo: float, s1_d1_hbr: float, s1_d2_hbr: float, s2_d1_hbr: float,
                 s2_d3_hbr: float, s3_d1_hbr: float, s3_d3_hbr: float, s3_d4_hbr: float, s4_d2_hbr: float,
                 s4_d4_hbr: float, s4_d5_hbr: float, s5_d3_hbr: float, s5_d4_hbr: float, s5_d6_hbr: float,
                 s6_d4_hbr: float, s6_d6_hbr: float, s6_d7_hbr: float, s7_d5_hbr: float, s7_d7_hbr: float,
                 s8_d6_hbr: float, s8_d7_hbr: float):
        super().__init__()

        self.group_session_id = group_session_id
        self.id = id
        self.task_id = task_id
        self.station_id = station_id
        self.participant_id = participant_id
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.s1_d1_hbo = s1_d1_hbo
        self.s1_d2_hbo = s1_d2_hbo
        self.s2_d1_hbo = s2_d1_hbo
        self.s2_d3_hbo = s2_d3_hbo
        self.s3_d1_hbo = s3_d1_hbo
        self.s3_d3_hbo = s3_d3_hbo
        self.s3_d4_hbo = s3_d4_hbo
        self.s4_d2_hbo = s4_d2_hbo
        self.s4_d4_hbo = s4_d4_hbo
        self.s4_d5_hbo = s4_d5_hbo
        self.s5_d3_hbo = s5_d3_hbo
        self.s5_d4_hbo = s5_d4_hbo
        self.s5_d6_hbo = s5_d6_hbo
        self.s6_d4_hbo = s6_d4_hbo
        self.s6_d6_hbo = s6_d6_hbo
        self.s6_d7_hbo = s6_d7_hbo
        self.s7_d5_hbo = s7_d5_hbo
        self.s7_d7_hbo = s7_d7_hbo
        self.s8_d6_hbo = s8_d6_hbo
        self.s8_d7_hbo = s8_d7_hbo
        self.s1_d1_hbr = s1_d1_hbr
        self.s1_d2_hbr = s1_d2_hbr
        self.s2_d1_hbr = s2_d1_hbr
        self.s2_d3_hbr = s2_d3_hbr
        self.s3_d1_hbr = s3_d1_hbr
        self.s3_d3_hbr = s3_d3_hbr
        self.s3_d4_hbr = s3_d4_hbr
        self.s4_d2_hbr = s4_d2_hbr
        self.s4_d4_hbr = s4_d4_hbr
        self.s4_d5_hbr = s4_d5_hbr
        self.s5_d3_hbr = s5_d3_hbr
        self.s5_d4_hbr = s5_d4_hbr
        self.s5_d6_hbr = s5_d6_hbr
        self.s6_d4_hbr = s6_d4_hbr
        self.s6_d6_hbr = s6_d6_hbr
        self.s6_d7_hbr = s6_d7_hbr
        self.s7_d5_hbr = s7_d5_hbr
        self.s7_d7_hbr = s7_d7_hbr
        self.s8_d6_hbr = s8_d6_hbr
        self.s8_d7_hbr = s8_d7_hbr

    @staticmethod
    def get_next_id(database_engine, group_session_id):
        with Session(database_engine) as session:
            max_id = session.query(func.max(FNIRSRaw.id)).filter_by(group_session_id=group_session_id).scalar()
            return max_id + 1 if max_id is not None else 1
