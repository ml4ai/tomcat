from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import REAL
from sqlalchemy import Text
from sqlalchemy import func
from sqlalchemy.orm import Mapped
from sqlalchemy.orm import Session
from sqlalchemy.orm import mapped_column

from datasette_interface.database.entity.base.base import Base


class GAZERaw(Base):
    __tablename__ = "gaze_raw"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"),
                                                  primary_key=True)
    station_id: Mapped[str] = mapped_column("station", Text, ForeignKey("station.id"), primary_key=True)
    participant_id: Mapped[int] = mapped_column("participant", Integer, ForeignKey("participant.id"), primary_key=True)
    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    task_id: Mapped[Optional[str]] = mapped_column("task", Text, ForeignKey("task.id"))
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

    @staticmethod
    def get_next_id(database_engine, group_session_id, station_id):
        with Session(database_engine) as session:
            max_id = session.query(func.max(GAZERaw.id)).filter_by(group_session_id=group_session_id,
                                                                    station_id=station_id).scalar()
            return max_id + 1 if max_id is not None else 1
