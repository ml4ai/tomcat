from sqlalchemy import REAL, ForeignKey, Integer, Text
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.station import Station


class EKGSync(Base):
    __tablename__ = "ekg_sync"

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
    aux_ekg: Mapped[float] = mapped_column(REAL)
