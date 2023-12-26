from typing import Optional

from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import Text

from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.station import Station
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.entity.base.task import Task
from datasette_interface.database.config import Base


class ScreenCapture(Base):
    __tablename__ = "screen_capture"

    group_session_id: Mapped[str] = mapped_column("group_session", Text,
                                                  ForeignKey(GroupSession.id),
                                                  primary_key=True)
    station_id: Mapped[str] = mapped_column("station", Text, ForeignKey(Station.id),
                                            primary_key=True)
    participant_id: Mapped[int] = mapped_column("participant", Integer,
                                                ForeignKey(Participant.id), primary_key=True)
    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    task_id: Mapped[Optional[str]] = mapped_column("task", Text, ForeignKey(Task.id))
    timestamp_unix: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    filename: Mapped[str] = mapped_column(Text)
    url: Mapped[str] = mapped_column(Text)
    timestamp_origin: Mapped[str] = mapped_column(Text)
