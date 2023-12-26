from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Boolean
from sqlalchemy import CheckConstraint
from sqlalchemy import Integer
from sqlalchemy import Text

from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.station import Station
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.config import Base


class PingPongCompetitiveTaskObservation(Base):
    __tablename__ = "ping_pong_competitive_task_observation"

    group_session_id: Mapped[str] = mapped_column("group_session", Text,
                                                  ForeignKey(GroupSession.id),
                                                  primary_key=True)
    player_1_id: Mapped[int] = mapped_column(Integer, ForeignKey(Participant.id), primary_key=True)
    player_2_id: Mapped[int] = mapped_column(Integer, ForeignKey(Participant.id), primary_key=True)
    player_1_station_id: Mapped[str] = mapped_column("player_1_station", Text,
                                                     ForeignKey(Station.id),
                                                     primary_key=True)
    player_2_station_id: Mapped[str] = mapped_column("player_2_station", Text,
                                                     ForeignKey(Station.id),
                                                     primary_key=True)
    timestamp_unix: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    task_started: Mapped[bool] = mapped_column(Boolean)
    seconds: Mapped[int] = mapped_column(Integer, CheckConstraint("seconds >= 0"))
    ball_position_x: Mapped[int] = mapped_column(Integer, CheckConstraint("ball_position_x >= 0"))
    ball_position_y: Mapped[int] = mapped_column(Integer, CheckConstraint("ball_position_y >= 0"))
    player_1_paddle_position_x: Mapped[int] = mapped_column(Integer, CheckConstraint(
        "player_1_paddle_position_x >= 0"))
    player_1_paddle_position_y: Mapped[int] = mapped_column(Integer, CheckConstraint(
        "player_1_paddle_position_y >= 0"))
    player_2_paddle_position_x: Mapped[int] = mapped_column(Integer, CheckConstraint(
        "player_2_paddle_position_x >= 0"))
    player_2_paddle_position_y: Mapped[int] = mapped_column(Integer, CheckConstraint(
        "player_2_paddle_position_y >= 0"))
    player_1_score: Mapped[int] = mapped_column(Integer, CheckConstraint("player_1_score >= 0"))
    player_2_score: Mapped[int] = mapped_column(Integer, CheckConstraint("player_2_score >= 0"))
