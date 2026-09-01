from typing import Any, Dict, Optional

from sqlalchemy import ForeignKey, Integer, Text, func
from sqlalchemy.dialects.postgresql import JSONB
from sqlalchemy.orm import Mapped, Session, mapped_column

from dataset_pipeline.database.config import Base
from dataset_pipeline.database.entity.base.group_session import GroupSession


class MinecraftMission(Base):
    __tablename__ = "minecraft_mission"

    id: Mapped[str] = mapped_column(Text, primary_key=True)
    group_session_id: Mapped[str] = mapped_column(
        "group_session", Text, ForeignKey(GroupSession.id)
    )
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


class MinecraftTestbedMessage(Base):
    __tablename__ = "minecraft_testbed_message"

    mission_id: Mapped[str] = mapped_column(
        "mission", Text, ForeignKey(MinecraftMission.id), primary_key=True
    )
    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    topic: Mapped[str] = mapped_column(Text)
    timestamp_unix: Mapped[str] = mapped_column(Text)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    # JSONB, not JSON: the payload is there to be queried into, and jsonb is what
    # supports the containment operators and GIN indexing that makes that practical.
    # Pass the dict itself -- SQLAlchemy serializes it. Passing json.dumps(...) here
    # stores a JSON *string* rather than an object, which is not an error and which
    # makes `message -> 'data'` return NULL instead of failing.
    message: Mapped[Dict[str, Any]] = mapped_column(JSONB)

    @staticmethod
    def get_next_id(database_engine, mission_id):
        with Session(database_engine) as session:
            max_id = (
                session.query(func.max(MinecraftTestbedMessage.id))
                .filter_by(mission_id=mission_id)
                .scalar()
            )
            return max_id + 1 if max_id is not None else 1
