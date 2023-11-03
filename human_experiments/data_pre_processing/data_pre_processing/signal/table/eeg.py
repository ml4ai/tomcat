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
                             primary_key=True)

    @declared_attr
    def station_id(cls) -> Mapped[str]:
        return mapped_column("station", Text, ForeignKey("station.id"),
                             primary_key=True)

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
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


class EEGSync(EEGMixin, Base):
    __tablename__ = "eeg_sync"

    def __init__(self,
                 group_session_id: str,
                 id: int,
                 task_id: Optional[str],
                 station_id: str,
                 participant_id: int,
                 timestamp_unix: str,
                 timestamp_iso8601: str,
                 aff1h: float,
                 f7: float,
                 fc5: float,
                 c3: float,
                 t7: float,
                 tp9: float,
                 pz: float,
                 p3: float,
                 p7: float,
                 o1: float,
                 o2: float,
                 p8: float,
                 p4: float,
                 tp10: float,
                 cz: float,
                 c4: float,
                 t8: float,
                 fc6: float,
                 fcz: float,
                 f8: float,
                 aff2h: float):
        super().__init__()

        self.group_session_id = group_session_id
        self.id = id
        self.task_id = task_id
        self.station_id = station_id
        self.participant_id = participant_id
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.aff1h = aff1h
        self.f7 = f7
        self.fc5 = fc5
        self.c3 = c3
        self.t7 = t7
        self.tp9 = tp9
        self.pz = pz
        self.p3 = p3
        self.p7 = p7
        self.o1 = o1
        self.o2 = o2
        self.p8 = p8
        self.p4 = p4
        self.tp10 = tp10
        self.cz = cz
        self.c4 = c4
        self.t8 = t8
        self.fc6 = fc6
        self.fcz = fcz
        self.f8 = f8
        self.aff2h = aff2h


class EEGFiltered(EEGMixin, Base):
    __tablename__ = "eeg_filtered"

    def __init__(self,
                 group_session_id: str,
                 id: int,
                 task_id: Optional[str],
                 station_id: str,
                 participant_id: int,
                 timestamp_unix: str,
                 timestamp_iso8601: str,
                 aff1h: float,
                 f7: float,
                 fc5: float,
                 c3: float,
                 t7: float,
                 tp9: float,
                 pz: float,
                 p3: float,
                 p7: float,
                 o1: float,
                 o2: float,
                 p8: float,
                 p4: float,
                 tp10: float,
                 cz: float,
                 c4: float,
                 t8: float,
                 fc6: float,
                 fcz: float,
                 f8: float,
                 aff2h: float):
        super().__init__()

        self.group_session_id = group_session_id
        self.id = id
        self.task_id = task_id
        self.station_id = station_id
        self.participant_id = participant_id
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.aff1h = aff1h
        self.f7 = f7
        self.fc5 = fc5
        self.c3 = c3
        self.t7 = t7
        self.tp9 = tp9
        self.pz = pz
        self.p3 = p3
        self.p7 = p7
        self.o1 = o1
        self.o2 = o2
        self.p8 = p8
        self.p4 = p4
        self.tp10 = tp10
        self.cz = cz
        self.c4 = c4
        self.t8 = t8
        self.fc6 = fc6
        self.fcz = fcz
        self.f8 = f8
        self.aff2h = aff2h
