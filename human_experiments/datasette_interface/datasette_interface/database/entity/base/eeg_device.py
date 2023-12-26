from sqlalchemy import ForeignKey
from sqlalchemy import Text
from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column

from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.station import Station
from datasette_interface.database.config import Base
from typing import Optional


class EEGDevice(Base):
    __tablename__ = "eeg_device"

    group_session_id: Mapped[str] = mapped_column("group_session", Text,
                                                  ForeignKey(GroupSession.id), primary_key=True)
    station_id: Mapped[str] = mapped_column("station", Text, ForeignKey(Station.id),
                                            primary_key=True)
    device_id: Mapped[Optional[str]] = mapped_column(Text)
