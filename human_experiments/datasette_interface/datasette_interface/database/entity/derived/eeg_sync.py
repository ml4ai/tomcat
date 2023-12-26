from sqlalchemy import REAL, ForeignKey, Integer, Text
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.station import Station


class EEGSync(Base):
    __tablename__ = "eeg_sync"

    group_session_id: Mapped[str] = mapped_column(
        "group_session", Text, ForeignKey(GroupSession.id), primary_key=True
    )
    frequency: Mapped[int] = mapped_column(Integer, primary_key=True)
    station_id: Mapped[str] = mapped_column(
        "station", Text, ForeignKey(Station.id), primary_key=True
    )
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
