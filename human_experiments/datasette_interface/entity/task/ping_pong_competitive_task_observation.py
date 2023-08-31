from typing import Optional

from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Boolean
from sqlalchemy import CheckConstraint
from sqlalchemy import Integer
from sqlalchemy import Text

from entity.base.base import Base


class PingPongCompetitiveTaskObservation(Base):
    __tablename__ = "ping_pong_competitive_task_observation"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"),
                                                  primary_key=True)
    player_1_id: Mapped[int] = mapped_column(Integer, ForeignKey("participant.id"), primary_key=True)
    player_2_id: Mapped[int] = mapped_column(Integer, ForeignKey("participant.id"), primary_key=True)
    player_1_station_id: Mapped[str] = mapped_column("player_1_station", Text, ForeignKey("station.id"),
                                                     primary_key=True)
    player_2_station_id: Mapped[str] = mapped_column("player_2_station", Text, ForeignKey("station.id"),
                                                     primary_key=True)
    timestamp_unix: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    task_started: Mapped[bool] = mapped_column(Boolean)
    seconds: Mapped[int] = mapped_column(Integer, CheckConstraint("seconds >= 0"))
    ball_position_x: Mapped[int] = mapped_column(Integer, CheckConstraint("ball_position_x >= 0"))
    ball_position_y: Mapped[int] = mapped_column(Integer, CheckConstraint("ball_position_y >= 0"))
    player_1_paddle_position_x: Mapped[int] = mapped_column(Integer, CheckConstraint("player_1_paddle_position_x >= 0"))
    player_1_paddle_position_y: Mapped[int] = mapped_column(Integer, CheckConstraint("player_1_paddle_position_y >= 0"))
    player_2_paddle_position_x: Mapped[int] = mapped_column(Integer, CheckConstraint("player_2_paddle_position_x >= 0"))
    player_2_paddle_position_y: Mapped[int] = mapped_column(Integer, CheckConstraint("player_2_paddle_position_y >= 0"))
    player_1_score: Mapped[int] = mapped_column(Integer, CheckConstraint("player_1_score >= 0"))
    player_2_score: Mapped[int] = mapped_column(Integer, CheckConstraint("player_2_score >= 0"))

    def __init__(self, group_session_id: str, player_1_id: int, player_2_id: int, player_1_station_id: str,
                 player_2_station_id: str, timestamp_unix: str, timestamp_iso8601: str, task_started: bool,
                 seconds: int, ball_position_x: int, ball_position_y: int, player_1_paddle_position_x: int,
                 player_1_paddle_position_y: int, player_2_paddle_position_x: int, player_2_paddle_position_y: int,
                 player_1_score: int, player_2_score: int):
        super().__init__()

        self.group_session_id = group_session_id
        self.player_1_id = player_1_id
        self.player_2_id = player_2_id
        self.player_1_station_id = player_1_station_id
        self.player_2_station_id = player_2_station_id
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.task_started = task_started
        self.seconds = seconds
        self.ball_position_x = ball_position_x
        self.ball_position_y = ball_position_y
        self.player_1_paddle_position_x = player_1_paddle_position_x
        self.player_1_paddle_position_y = player_1_paddle_position_y
        self.player_2_paddle_position_x = player_2_paddle_position_x
        self.player_2_paddle_position_y = player_2_paddle_position_y
        self.player_1_score = player_1_score
        self.player_2_score = player_2_score
