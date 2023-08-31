from typing import Any
from typing import Dict

from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import JSON
from sqlalchemy import Text

from entity.base.base import Base


class MinecraftMission(Base):
    __tablename__ = "minecraft_mission"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"),
                                                  primary_key=True)
    id: Mapped[str] = mapped_column(Text, primary_key=True)
    name: Mapped[str] = mapped_column(Text)
    start_timestamp_unix: Mapped[str] = mapped_column(Text)
    start_timestamp_iso8601: Mapped[str] = mapped_column(Text)
    stop_timestamp_unix: Mapped[str] = mapped_column(Text)
    stop_timestamp_iso8601: Mapped[str] = mapped_column(Text)
    final_team_score: Mapped[int] = mapped_column(Integer)
    testbed_version: Mapped[str] = mapped_column(Text)

    def __init__(self, group_session_id: str, id: str, name: str, start_timestamp_unix: str,
                 start_timestamp_iso8601: str, stop_timestamp_unix: str, stop_timestamp_iso8601: str,
                 final_team_score: int, testbed_version: str):
        super().__init__()

        self.group_session_id = group_session_id
        self.id = id
        self.name = name
        self.start_timestamp_unix = start_timestamp_unix
        self.start_timestamp_iso8601 = start_timestamp_iso8601
        self.stop_timestamp_unix = stop_timestamp_unix
        self.stop_timestamp_iso8601 = stop_timestamp_iso8601
        self.final_team_score = final_team_score
        self.testbed_version = testbed_version


class MinecraftTestbedMessage(Base):
    __tablename__ = "minecraft_testbed_message"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"),
                                                  primary_key=True)
    mission_id: Mapped[str] = mapped_column("mission", Text, ForeignKey("minecraft_mission.id"), primary_key=True)
    topic: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_unix: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    message: Mapped[Dict[str, Any]] = mapped_column(JSON)

    def __init__(self, group_session_id: str, mission_id: str, topic: str, timestamp_unix: str, timestamp_iso8601: str,
                 message: str):
        super().__init__()

        self.group_session_id = group_session_id
        self.mission_id = mission_id
        self.topic = topic
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.message = message
