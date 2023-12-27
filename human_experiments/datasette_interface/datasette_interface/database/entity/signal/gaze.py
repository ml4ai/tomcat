from typing import Optional

from sqlalchemy import REAL, ForeignKey, Integer, Text
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.entity.base.station import Station
from datasette_interface.database.entity.base.task import Task


class GAZERaw(Base):
    __tablename__ = "gaze_raw"

    group_session_id: Mapped[str] = mapped_column(
        "group_session", Text, ForeignKey(GroupSession.id), primary_key=True
    )
    station_id: Mapped[str] = mapped_column(
        "station", Text, ForeignKey(Station.id), primary_key=True
    )
    participant_id: Mapped[int] = mapped_column(
        "participant", Integer, ForeignKey(Participant.id), primary_key=True
    )
    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    task_id: Mapped[Optional[str]] = mapped_column(
        "task", Text, ForeignKey(Task.id), nullable=True
    )
    timestamp_unix: Mapped[str] = mapped_column(Text)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    confidence: Mapped[float] = mapped_column(REAL)
    norm_pos_x: Mapped[float] = mapped_column(REAL)
    norm_pos_y: Mapped[float] = mapped_column(REAL)
    gaze_point_3d_x: Mapped[float] = mapped_column(REAL)
    gaze_point_3d_y: Mapped[float] = mapped_column(REAL)
    gaze_point_3d_z: Mapped[float] = mapped_column(REAL)
    eye_center0_3d_x: Mapped[float] = mapped_column(REAL)
    eye_center0_3d_y: Mapped[float] = mapped_column(REAL)
    eye_center0_3d_z: Mapped[float] = mapped_column(REAL)
    eye_center1_3d_x: Mapped[float] = mapped_column(REAL)
    eye_center1_3d_y: Mapped[float] = mapped_column(REAL)
    eye_center1_3d_z: Mapped[float] = mapped_column(REAL)
    gaze_normal0_x: Mapped[float] = mapped_column(REAL)
    gaze_normal0_y: Mapped[float] = mapped_column(REAL)
    gaze_normal0_z: Mapped[float] = mapped_column(REAL)
    gaze_normal1_x: Mapped[float] = mapped_column(REAL)
    gaze_normal1_y: Mapped[float] = mapped_column(REAL)
    gaze_normal1_z: Mapped[float] = mapped_column(REAL)
    diameter0_2d: Mapped[float] = mapped_column(REAL)
    diameter1_2d: Mapped[float] = mapped_column(REAL)
    diameter0_3d: Mapped[float] = mapped_column(REAL)
    diameter1_3d: Mapped[float] = mapped_column(REAL)
