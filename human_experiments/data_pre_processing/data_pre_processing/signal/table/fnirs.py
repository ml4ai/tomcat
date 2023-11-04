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

    frequency: Mapped[int] = mapped_column(Integer, primary_key=True, sort_order=1)

    @declared_attr
    def station(cls) -> Mapped[str]:
        return mapped_column("station", Text, ForeignKey("station.id"),
                             primary_key=True, sort_order=2)

    id: Mapped[int] = mapped_column(Integer, primary_key=True, sort_order=3)
    timestamp_unix: Mapped[str] = mapped_column(Text, sort_order=4)
    timestamp_iso8601: Mapped[str] = mapped_column(Text, sort_order=5)
    s1_d1_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s1_d2_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s2_d1_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s2_d3_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s3_d1_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s3_d3_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s3_d4_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s4_d2_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s4_d4_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s4_d5_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s5_d3_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s5_d4_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s5_d6_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s6_d4_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s6_d6_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s6_d7_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s7_d5_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s7_d7_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s8_d6_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s8_d7_hbo: Mapped[float] = mapped_column(REAL, sort_order=6)
    s1_d1_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s1_d2_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s2_d1_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s2_d3_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s3_d1_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s3_d3_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s3_d4_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s4_d2_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s4_d4_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s4_d5_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s5_d3_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s5_d4_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s5_d6_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s6_d4_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s6_d6_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s6_d7_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s7_d5_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s7_d7_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s8_d6_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)
    s8_d7_hbr: Mapped[float] = mapped_column(REAL, sort_order=6)


class FNIRSSyncUnfiltered(FNIRSMixin, Base):
    __tablename__ = "fnirs_sync_unfiltered"


class FNIRSSyncFiltered(FNIRSMixin, Base):
    __tablename__ = "fnirs_sync_filtered"
