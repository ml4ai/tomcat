from typing import Optional

from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import Text

from entity.base.base import Base


class ScreenCapture(Base):
    __tablename__ = "screen_capture"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"),
                                                  primary_key=True)
    station_id: Mapped[str] = mapped_column("station", Text, ForeignKey("station.id"),
                                            primary_key=True)
    participant_id: Mapped[int] = mapped_column("participant", Integer,
                                                ForeignKey("participant.id"), primary_key=True)
    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    task_id: Mapped[Optional[str]] = mapped_column("task", Text, ForeignKey("task.id"))
    timestamp_unix: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    filename: Mapped[str] = mapped_column(Text)
    timestamp_origin: Mapped[str] = mapped_column(Text)

    def __init__(self, group_session_id: str,
                 id: int,
                 task_id: Optional[str],
                 station_id: str,
                 participant_id: int,
                 timestamp_unix: str,
                 timestamp_iso8601: str,
                 filename: int,
                 timestamp_origin: str):
        super().__init__()

        self.group_session_id = group_session_id
        self.id = id
        self.task_id = task_id
        self.station_id = station_id
        self.participant_id = participant_id
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.filename = filename
        self.timestamp_origin = timestamp_origin
