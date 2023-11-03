from typing import Optional

from sqlalchemy.orm import declarative_mixin, declared_attr
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import REAL
from sqlalchemy import Text
from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column

from data_pre_processing.signal.table.base import Base


@declarative_mixin
class FNIRSMixin:

    @declared_attr
    def group_session(cls) -> Mapped[str]:
        return mapped_column("group_session", Text, ForeignKey("group_session.id"),
                             primary_key=True, sort_order=0)

    @declared_attr
    def station(cls) -> Mapped[str]:
        return mapped_column("station", Text, ForeignKey("station.id"),
                             primary_key=True, sort_order=1)

    id: Mapped[int] = mapped_column(Integer, primary_key=True, sort_order=2)
    timestamp_unix: Mapped[str] = mapped_column(Text, sort_order=3)
    timestamp_iso8601: Mapped[str] = mapped_column(Text, sort_order=4)
    s1_d1_hbo: Mapped[float] = mapped_column(REAL, sort_order=5)
    s1_d2_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s2_d1_hbo: Mapped[float] = mapped_column(REAL, sort_order=7)
    s2_d3_hbo: Mapped[float] = mapped_column(REAL, sort_order=8)
    s3_d1_hbo: Mapped[float] = mapped_column(REAL, sort_order=9)
    s3_d3_hbo: Mapped[float] = mapped_column(REAL, sort_order=10)
    s3_d4_hbo: Mapped[float] = mapped_column(REAL, sort_order=11)
    s4_d2_hbo: Mapped[float] = mapped_column(REAL, sort_order=12)
    s4_d4_hbo: Mapped[float] = mapped_column(REAL, sort_order=13)
    s4_d5_hbo: Mapped[float] = mapped_column(REAL, sort_order=14)
    s5_d3_hbo: Mapped[float] = mapped_column(REAL, sort_order=15)
    s5_d4_hbo: Mapped[float] = mapped_column(REAL, sort_order=16)
    s5_d6_hbo: Mapped[float] = mapped_column(REAL, sort_order=17)
    s6_d4_hbo: Mapped[float] = mapped_column(REAL, sort_order=18)
    s6_d6_hbo: Mapped[float] = mapped_column(REAL, sort_order=19)
    s6_d7_hbo: Mapped[float] = mapped_column(REAL, sort_order=20)
    s7_d5_hbo: Mapped[float] = mapped_column(REAL, sort_order=21)
    s7_d7_hbo: Mapped[float] = mapped_column(REAL, sort_order=22)
    s8_d6_hbo: Mapped[float] = mapped_column(REAL, sort_order=23)
    s8_d7_hbo: Mapped[float] = mapped_column(REAL, sort_order=24)
    s1_d1_hbr: Mapped[float] = mapped_column(REAL, sort_order=25)
    s1_d2_hbr: Mapped[float] = mapped_column(REAL, sort_order=26)
    s2_d1_hbr: Mapped[float] = mapped_column(REAL, sort_order=27)
    s2_d3_hbr: Mapped[float] = mapped_column(REAL, sort_order=28)
    s3_d1_hbr: Mapped[float] = mapped_column(REAL, sort_order=29)
    s3_d3_hbr: Mapped[float] = mapped_column(REAL, sort_order=30)
    s3_d4_hbr: Mapped[float] = mapped_column(REAL, sort_order=31)
    s4_d2_hbr: Mapped[float] = mapped_column(REAL, sort_order=32)
    s4_d4_hbr: Mapped[float] = mapped_column(REAL, sort_order=33)
    s4_d5_hbr: Mapped[float] = mapped_column(REAL, sort_order=34)
    s5_d3_hbr: Mapped[float] = mapped_column(REAL, sort_order=35)
    s5_d4_hbr: Mapped[float] = mapped_column(REAL, sort_order=36)
    s5_d6_hbr: Mapped[float] = mapped_column(REAL, sort_order=37)
    s6_d4_hbr: Mapped[float] = mapped_column(REAL, sort_order=38)
    s6_d6_hbr: Mapped[float] = mapped_column(REAL, sort_order=39)
    s6_d7_hbr: Mapped[float] = mapped_column(REAL, sort_order=40)
    s7_d5_hbr: Mapped[float] = mapped_column(REAL, sort_order=41)
    s7_d7_hbr: Mapped[float] = mapped_column(REAL, sort_order=42)
    s8_d6_hbr: Mapped[float] = mapped_column(REAL, sort_order=43)
    s8_d7_hbr: Mapped[float] = mapped_column(REAL, sort_order=44)


class FNIRSSync(FNIRSMixin, Base):
    __tablename__ = "fnirs_sync"

    # def __init__(self,
    #              group_session: str,
    #              id: int,
    #              station: str,
    #              timestamp_unix: str,
    #              timestamp_iso8601: str,
    #              s1_d1_hbo: float,
    #              s1_d2_hbo: float,
    #              s2_d1_hbo: float,
    #              s2_d3_hbo: float,
    #              s3_d1_hbo: float,
    #              s3_d3_hbo: float,
    #              s3_d4_hbo: float,
    #              s4_d2_hbo: float,
    #              s4_d4_hbo: float,
    #              s4_d5_hbo: float,
    #              s5_d3_hbo: float,
    #              s5_d4_hbo: float,
    #              s5_d6_hbo: float,
    #              s6_d4_hbo: float,
    #              s6_d6_hbo: float,
    #              s6_d7_hbo: float,
    #              s7_d5_hbo: float,
    #              s7_d7_hbo: float,
    #              s8_d6_hbo: float,
    #              s8_d7_hbo: float,
    #              s1_d1_hbr: float,
    #              s1_d2_hbr: float,
    #              s2_d1_hbr: float,
    #              s2_d3_hbr: float,
    #              s3_d1_hbr: float,
    #              s3_d3_hbr: float,
    #              s3_d4_hbr: float,
    #              s4_d2_hbr: float,
    #              s4_d4_hbr: float,
    #              s4_d5_hbr: float,
    #              s5_d3_hbr: float,
    #              s5_d4_hbr: float,
    #              s5_d6_hbr: float,
    #              s6_d4_hbr: float,
    #              s6_d6_hbr: float,
    #              s6_d7_hbr: float,
    #              s7_d5_hbr: float,
    #              s7_d7_hbr: float,
    #              s8_d6_hbr: float,
    #              s8_d7_hbr: float):
    #     super().__init__()
    #
    #     self.group_session_id = group_session
    #     self.id = id
    #     self.station_id = station
    #     self.timestamp_unix = timestamp_unix
    #     self.timestamp_iso8601 = timestamp_iso8601
    #     self.s1_d1_hbo = s1_d1_hbo
    #     self.s1_d2_hbo = s1_d2_hbo
    #     self.s2_d1_hbo = s2_d1_hbo
    #     self.s2_d3_hbo = s2_d3_hbo
    #     self.s3_d1_hbo = s3_d1_hbo
    #     self.s3_d3_hbo = s3_d3_hbo
    #     self.s3_d4_hbo = s3_d4_hbo
    #     self.s4_d2_hbo = s4_d2_hbo
    #     self.s4_d4_hbo = s4_d4_hbo
    #     self.s4_d5_hbo = s4_d5_hbo
    #     self.s5_d3_hbo = s5_d3_hbo
    #     self.s5_d4_hbo = s5_d4_hbo
    #     self.s5_d6_hbo = s5_d6_hbo
    #     self.s6_d4_hbo = s6_d4_hbo
    #     self.s6_d6_hbo = s6_d6_hbo
    #     self.s6_d7_hbo = s6_d7_hbo
    #     self.s7_d5_hbo = s7_d5_hbo
    #     self.s7_d7_hbo = s7_d7_hbo
    #     self.s8_d6_hbo = s8_d6_hbo
    #     self.s8_d7_hbo = s8_d7_hbo
    #     self.s1_d1_hbr = s1_d1_hbr
    #     self.s1_d2_hbr = s1_d2_hbr
    #     self.s2_d1_hbr = s2_d1_hbr
    #     self.s2_d3_hbr = s2_d3_hbr
    #     self.s3_d1_hbr = s3_d1_hbr
    #     self.s3_d3_hbr = s3_d3_hbr
    #     self.s3_d4_hbr = s3_d4_hbr
    #     self.s4_d2_hbr = s4_d2_hbr
    #     self.s4_d4_hbr = s4_d4_hbr
    #     self.s4_d5_hbr = s4_d5_hbr
    #     self.s5_d3_hbr = s5_d3_hbr
    #     self.s5_d4_hbr = s5_d4_hbr
    #     self.s5_d6_hbr = s5_d6_hbr
    #     self.s6_d4_hbr = s6_d4_hbr
    #     self.s6_d6_hbr = s6_d6_hbr
    #     self.s6_d7_hbr = s6_d7_hbr
    #     self.s7_d5_hbr = s7_d5_hbr
    #     self.s7_d7_hbr = s7_d7_hbr
    #     self.s8_d6_hbr = s8_d6_hbr
    #     self.s8_d7_hbr = s8_d7_hbr


class FNIRSFiltered(FNIRSMixin, Base):
    __tablename__ = "fnirs_filtered"

    def __init__(self,
                 group_session: str,
                 id: int,
                 station: str,
                 timestamp_unix: str,
                 timestamp_iso8601: str,
                 s1_d1_hbo: float,
                 s1_d2_hbo: float,
                 s2_d1_hbo: float,
                 s2_d3_hbo: float,
                 s3_d1_hbo: float,
                 s3_d3_hbo: float,
                 s3_d4_hbo: float,
                 s4_d2_hbo: float,
                 s4_d4_hbo: float,
                 s4_d5_hbo: float,
                 s5_d3_hbo: float,
                 s5_d4_hbo: float,
                 s5_d6_hbo: float,
                 s6_d4_hbo: float,
                 s6_d6_hbo: float,
                 s6_d7_hbo: float,
                 s7_d5_hbo: float,
                 s7_d7_hbo: float,
                 s8_d6_hbo: float,
                 s8_d7_hbo: float,
                 s1_d1_hbr: float,
                 s1_d2_hbr: float,
                 s2_d1_hbr: float,
                 s2_d3_hbr: float,
                 s3_d1_hbr: float,
                 s3_d3_hbr: float,
                 s3_d4_hbr: float,
                 s4_d2_hbr: float,
                 s4_d4_hbr: float,
                 s4_d5_hbr: float,
                 s5_d3_hbr: float,
                 s5_d4_hbr: float,
                 s5_d6_hbr: float,
                 s6_d4_hbr: float,
                 s6_d6_hbr: float,
                 s6_d7_hbr: float,
                 s7_d5_hbr: float,
                 s7_d7_hbr: float,
                 s8_d6_hbr: float,
                 s8_d7_hbr: float):
        super().__init__()

        self.group_session_id = group_session
        self.id = id
        self.station_id = station
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
