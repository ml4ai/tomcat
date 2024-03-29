from typing import Optional

from sqlalchemy import Boolean, CheckConstraint, ForeignKey, Integer, Text
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.participant import Participant


class PingPongCooperativeTaskObservation(Base):
    __tablename__ = "ping_pong_cooperative_task_observation"

    group_session_id: Mapped[str] = mapped_column(
        "group_session", Text, ForeignKey(GroupSession.id), primary_key=True
    )
    player_1_id: Mapped[int] = mapped_column(
        Integer, ForeignKey(Participant.id), primary_key=True
    )
    player_2_id: Mapped[int] = mapped_column(
        Integer, ForeignKey(Participant.id), primary_key=True
    )
    player_3_id: Mapped[int] = mapped_column(
        Integer, ForeignKey(Participant.id), primary_key=True
    )
    timestamp_unix: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    task_started: Mapped[bool] = mapped_column(Boolean)
    seconds: Mapped[int] = mapped_column(Integer, CheckConstraint("seconds >= 0"))
    ball_position_x: Mapped[int] = mapped_column(
        Integer, CheckConstraint("ball_position_x >= 0")
    )
    ball_position_y: Mapped[int] = mapped_column(
        Integer, CheckConstraint("ball_position_y >= 0")
    )
    player_1_paddle_position_x: Mapped[int] = mapped_column(
        Integer, CheckConstraint("player_1_paddle_position_x >= 0")
    )
    player_1_paddle_position_y: Mapped[int] = mapped_column(
        Integer, CheckConstraint("player_1_paddle_position_y >= 0")
    )
    player_2_paddle_position_x: Mapped[int] = mapped_column(
        Integer, CheckConstraint("player_2_paddle_position_x >= 0")
    )
    player_2_paddle_position_y: Mapped[int] = mapped_column(
        Integer, CheckConstraint("player_2_paddle_position_y >= 0")
    )
    player_3_paddle_position_x: Mapped[int] = mapped_column(
        Integer, CheckConstraint("player_3_paddle_position_x >= 0")
    )
    player_3_paddle_position_y: Mapped[int] = mapped_column(
        Integer, CheckConstraint("player_3_paddle_position_y >= 0")
    )
    ai_paddle_position_x: Mapped[Optional[int]] = mapped_column(
        Integer, CheckConstraint("ai_paddle_position_x >= 0")
    )
    ai_paddle_position_y: Mapped[Optional[int]] = mapped_column(
        Integer, CheckConstraint("ai_paddle_position_y >= 0")
    )
    team_score: Mapped[int] = mapped_column(Integer, CheckConstraint("team_score >= 0"))
    ai_score: Mapped[int] = mapped_column(Integer, CheckConstraint("ai_score >= 0"))
