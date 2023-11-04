from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import REAL
from sqlalchemy import Text
from sqlalchemy.orm import Mapped, mapped_column, declarative_mixin, declared_attr


from data_pre_processing.signal.table.base import Base


@declarative_mixin
class EEGMixin:

    @declared_attr
    def group_session_id(cls) -> Mapped[str]:
        return mapped_column("group_session", Text, ForeignKey("group_session.id"),
                             primary_key=True, sort_order=0)

    frequency: Mapped[int] = mapped_column(Integer, primary_key=True, sort_order=1)

    @declared_attr
    def station_id(cls) -> Mapped[str]:
        return mapped_column("station", Text, ForeignKey("station.id"),
                             primary_key=True, sort_order=2)

    id: Mapped[int] = mapped_column(Integer, primary_key=True, sort_order=3)
    timestamp_unix: Mapped[str] = mapped_column(Text, sort_order=4)
    timestamp_iso8601: Mapped[str] = mapped_column(Text, sort_order=5)
    aff1h: Mapped[float] = mapped_column(REAL, sort_order=6)
    f7: Mapped[float] = mapped_column(REAL, sort_order=6)
    fc5: Mapped[float] = mapped_column(REAL, sort_order=6)
    c3: Mapped[float] = mapped_column(REAL, sort_order=6)
    t7: Mapped[float] = mapped_column(REAL, sort_order=6)
    tp9: Mapped[float] = mapped_column(REAL, sort_order=6)
    pz: Mapped[float] = mapped_column(REAL, sort_order=6)
    p3: Mapped[float] = mapped_column(REAL, sort_order=6)
    p7: Mapped[float] = mapped_column(REAL, sort_order=6)
    o1: Mapped[float] = mapped_column(REAL, sort_order=6)
    o2: Mapped[float] = mapped_column(REAL, sort_order=6)
    p8: Mapped[float] = mapped_column(REAL, sort_order=6)
    p4: Mapped[float] = mapped_column(REAL, sort_order=6)
    tp10: Mapped[float] = mapped_column(REAL, sort_order=6)
    cz: Mapped[float] = mapped_column(REAL, sort_order=6)
    c4: Mapped[float] = mapped_column(REAL, sort_order=6)
    t8: Mapped[float] = mapped_column(REAL, sort_order=6)
    fc6: Mapped[float] = mapped_column(REAL, sort_order=6)
    fcz: Mapped[float] = mapped_column(REAL, sort_order=6)
    f8: Mapped[float] = mapped_column(REAL, sort_order=6)
    aff2h: Mapped[float] = mapped_column(REAL, sort_order=6)


class EEGSyncUnfiltered(EEGMixin, Base):
    __tablename__ = "eeg_sync_unfiltered"


class EEGSyncFiltered(EEGMixin, Base):
    __tablename__ = "eeg_sync_filtered"

