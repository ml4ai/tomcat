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


class EEGRaw(Base):
    __tablename__ = "eeg_raw"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"),
                                                  primary_key=True)
    station_id: Mapped[str] = mapped_column("station", Text, ForeignKey("station.id"), primary_key=True)
    participant_id: Mapped[int] = mapped_column("participant", Integer, ForeignKey("participant.id"), primary_key=True)
    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    task_id: Mapped[Optional[str]] = mapped_column("task", Text, ForeignKey("task.id"))
    timestamp_unix: Mapped[str] = mapped_column(Text)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    aff1h: Mapped[float] = mapped_column(REAL)
    aff5h: Mapped[float] = mapped_column(REAL)
    f7: Mapped[float] = mapped_column(REAL)
    fc5: Mapped[float] = mapped_column(REAL)
    fc1: Mapped[float] = mapped_column(REAL)
    c3: Mapped[float] = mapped_column(REAL)
    t7: Mapped[float] = mapped_column(REAL)
    tp9: Mapped[float] = mapped_column(REAL)
    cp5: Mapped[float] = mapped_column(REAL)
    cp1: Mapped[float] = mapped_column(REAL)
    pz: Mapped[float] = mapped_column(REAL)
    p3: Mapped[float] = mapped_column(REAL)
    p7: Mapped[float] = mapped_column(REAL)
    po9: Mapped[float] = mapped_column(REAL)
    o1: Mapped[float] = mapped_column(REAL)
    oz: Mapped[float] = mapped_column(REAL)
    o2: Mapped[float] = mapped_column(REAL)
    po10: Mapped[float] = mapped_column(REAL)
    p8: Mapped[float] = mapped_column(REAL)
    p4: Mapped[float] = mapped_column(REAL)
    tp10: Mapped[float] = mapped_column(REAL)
    cp6: Mapped[float] = mapped_column(REAL)
    cp2: Mapped[float] = mapped_column(REAL)
    cz: Mapped[float] = mapped_column(REAL)
    c4: Mapped[float] = mapped_column(REAL)
    t8: Mapped[float] = mapped_column(REAL)
    fc6: Mapped[float] = mapped_column(REAL)
    fc2: Mapped[float] = mapped_column(REAL)
    fcz: Mapped[float] = mapped_column(REAL)
    f8: Mapped[float] = mapped_column(REAL)
    aff6h: Mapped[float] = mapped_column(REAL)
    aff2h: Mapped[float] = mapped_column(REAL)
    aux_gsr: Mapped[float] = mapped_column(REAL)
    aux_ekg: Mapped[float] = mapped_column(REAL)

    def __init__(self, group_session_id: str, id: int, task_id: Optional[str], station_id: str, participant_id: int,
                 timestamp_unix: str, timestamp_iso8601: str, aff1h: float, aff5h: float, f7: float, fc5: float,
                 fc1: float, c3: float, t7: float, tp9: float, cp5: float, cp1: float, pz: float, p3: float, p7: float,
                 po9: float, o1: float, oz: float, o2: float, po10: float, p8: float, p4: float, tp10: float,
                 cp6: float, cp2: float, cz: float, c4: float, t8: float, fc6: float, fc2: float, fcz: float, f8: float,
                 aff6h: float, aff2h: float, aux_gsr: float, aux_ekg: float):
        super().__init__()

        self.group_session_id = group_session_id
        self.id = id
        self.task_id = task_id
        self.station_id = station_id
        self.participant_id = participant_id
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.aff1h = aff1h
        self.aff5h = aff5h
        self.f7 = f7
        self.fc5 = fc5
        self.fc1 = fc1
        self.c3 = c3
        self.t7 = t7
        self.tp9 = tp9
        self.cp5 = cp5
        self.cp1 = cp1
        self.pz = pz
        self.p3 = p3
        self.p7 = p7
        self.po9 = po9
        self.o1 = o1
        self.oz = oz
        self.o2 = o2
        self.po10 = po10
        self.p8 = p8
        self.p4 = p4
        self.tp10 = tp10
        self.cp6 = cp6
        self.cp2 = cp2
        self.cz = cz
        self.c4 = c4
        self.t8 = t8
        self.fc6 = fc6
        self.fc2 = fc2
        self.fcz = fcz
        self.f8 = f8
        self.aff6h = aff6h
        self.aff2h = aff2h
        self.aux_gsr = aux_gsr
        self.aux_ekg = aux_ekg

    @staticmethod
    def get_next_id(database_engine, group_session_id, station_id):
        with Session(database_engine) as session:
            max_id = session.query(func.max(EEGRaw.id)).filter_by(group_session_id=group_session_id,
                                                                  station_id=station_id).scalar()
            return max_id + 1 if max_id is not None else 1
