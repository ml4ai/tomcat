from typing import Any
from typing import Dict
from typing import Optional

from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import JSON
from sqlalchemy import Text
from sqlalchemy import func
from sqlalchemy.orm import Session

from entity.base.base import Base


class MinecraftMission(Base):
    __tablename__ = "minecraft_mission"

    id: Mapped[str] = mapped_column(Text, primary_key=True)
    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"))
    name: Mapped[str] = mapped_column(Text)
    mission_start_timestamp_unix: Mapped[str] = mapped_column(Text)
    mission_start_timestamp_iso8601: Mapped[str] = mapped_column(Text)
    mission_stop_timestamp_unix: Mapped[str] = mapped_column(Text)
    mission_stop_timestamp_iso8601: Mapped[str] = mapped_column(Text)
    trial_start_timestamp_unix: Mapped[str] = mapped_column(Text)
    trial_start_timestamp_iso8601: Mapped[str] = mapped_column(Text)
    trial_stop_timestamp_unix: Mapped[str] = mapped_column(Text)
    trial_stop_timestamp_iso8601: Mapped[str] = mapped_column(Text)
    final_team_score: Mapped[Optional[int]] = mapped_column(Integer)
    testbed_version: Mapped[str] = mapped_column(Text)

    def __init__(self,
                 id: str,
                 group_session_id: str,
                 name: str,
                 mission_start_timestamp_unix: str,
                 mission_start_timestamp_iso8601: str,
                 mission_stop_timestamp_unix: str,
                 mission_stop_timestamp_iso8601: str,
                 trial_start_timestamp_unix: str,
                 trial_start_timestamp_iso8601: str,
                 trial_stop_timestamp_unix: str,
                 trial_stop_timestamp_iso8601: str,
                 final_team_score: int,
                 testbed_version: str):
        super().__init__()

        self.id = id
        self.group_session_id = group_session_id
        self.name = name
        self.mission_start_timestamp_unix = mission_start_timestamp_unix
        self.mission_start_timestamp_iso8601 = mission_start_timestamp_iso8601
        self.mission_stop_timestamp_unix = mission_stop_timestamp_unix
        self.mission_stop_timestamp_iso8601 = mission_stop_timestamp_iso8601
        self.trial_start_timestamp_unix = trial_start_timestamp_unix
        self.trial_start_timestamp_iso8601 = trial_start_timestamp_iso8601
        self.trial_stop_timestamp_unix = trial_stop_timestamp_unix
        self.trial_stop_timestamp_iso8601 = trial_stop_timestamp_iso8601
        self.final_team_score = final_team_score
        self.testbed_version = testbed_version


class MinecraftTestbedMessage(Base):
    __tablename__ = "minecraft_testbed_message"

    mission_id: Mapped[str] = mapped_column("mission", Text, ForeignKey("minecraft_mission.id"), primary_key=True)
    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    topic: Mapped[str] = mapped_column(Text)
    timestamp_unix: Mapped[str] = mapped_column(Text)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    message: Mapped[Dict[str, Any]] = mapped_column(JSON)

    def __init__(self, mission_id: str, id: int, topic: str, timestamp_unix: str, timestamp_iso8601: str, message: str):
        super().__init__()

        self.mission_id = mission_id
        self.id = id
        self.topic = topic
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.message = message

    @staticmethod
    def get_next_id(database_engine, mission_id):
        with Session(database_engine) as session:
            max_id = session.query(func.max(MinecraftTestbedMessage.id)).filter_by(mission_id = mission_id).scalar()
            return max_id + 1 if max_id is not None else 1
