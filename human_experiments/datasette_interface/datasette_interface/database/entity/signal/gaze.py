
from sqlalchemy import REAL, ForeignKey, Integer, Text
from sqlalchemy.orm import (Mapped, declarative_mixin, declared_attr, mapped_column)

from datasette_interface.database.config import Base
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.entity.base.task import Task


@declarative_mixin
class GazeMixin:
    @declared_attr
    def group_session_id(cls) -> Mapped[str]:
        return mapped_column(
            "group_session",
            Text,
            ForeignKey("group_session.id"),
            primary_key=True,
            sort_order=0,
        )

    @declared_attr
    def station_id(cls) -> Mapped[str]:
        return mapped_column(
            "station", Text, ForeignKey("station.id"), primary_key=True, sort_order=2
        )

    @declared_attr
    def participant_id(cls) -> Mapped[int]:
        return mapped_column(
            "participant",
            Integer,
            ForeignKey(Participant.id),
            primary_key=True,
            sort_order=3,
        )

    id: Mapped[int] = mapped_column(Integer, primary_key=True, sort_order=4)

    @declared_attr
    def task_id(cls) -> Mapped[int]:
        return mapped_column(
            "task", Text, ForeignKey(Task.id), nullable=True, sort_order=5
        )

    timestamp_unix: Mapped[str] = mapped_column(Text, sort_order=6)
    timestamp_iso8601: Mapped[str] = mapped_column(Text, sort_order=7)
    confidence: Mapped[float] = mapped_column(REAL, sort_order=8)
    norm_pos_x: Mapped[float] = mapped_column(REAL, sort_order=8)
    norm_pos_y: Mapped[float] = mapped_column(REAL, sort_order=8)
    gaze_point_3d_x: Mapped[float] = mapped_column(REAL, sort_order=8)
    gaze_point_3d_y: Mapped[float] = mapped_column(REAL, sort_order=8)
    gaze_point_3d_z: Mapped[float] = mapped_column(REAL, sort_order=8)
    eye_center_3d_x: Mapped[float] = mapped_column(REAL, sort_order=8)
    eye_center_3d_y: Mapped[float] = mapped_column(REAL, sort_order=8)
    eye_center_3d_z: Mapped[float] = mapped_column(REAL, sort_order=8)
    gaze_normal_x: Mapped[float] = mapped_column(REAL, sort_order=8)
    gaze_normal_y: Mapped[float] = mapped_column(REAL, sort_order=8)
    gaze_normal_z: Mapped[float] = mapped_column(REAL, sort_order=8)
    diameter_2d: Mapped[float] = mapped_column(REAL, sort_order=8)
    diameter_3d: Mapped[float] = mapped_column(REAL, sort_order=8)


class GazeEye0Raw(GazeMixin, Base):
    __tablename__ = "gaze_eye0_raw"


class GazeEye1Raw(GazeMixin, Base):
    __tablename__ = "gaze_eye1_raw"
