from sqlalchemy import ForeignKey
from sqlalchemy import Text
from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column

from entity.base.base import Base
from typing import Optional

class EEGDevice(Base):
    __tablename__ = "eeg_device"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"), primary_key=True)
    station_id: Mapped[str] = mapped_column("station", Text, ForeignKey("station.id"), primary_key=True)
    device_id: Mapped[Optional[str]] = mapped_column(Text, primary_key=True)

    def __init__(self, group_session_id: str, station_id: str, device_id: str):
        super().__init__()

        self.group_session_id = group_session_id
        self.station_id = station_id
        self.device_id = device_id
