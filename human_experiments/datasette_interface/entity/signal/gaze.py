from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import REAL
from sqlalchemy import Text
from sqlalchemy import func
from sqlalchemy.orm import Mapped
from sqlalchemy.orm import Session
from sqlalchemy.orm import mapped_column

from entity.base.base import Base


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

    def __init__(self, group_session_id: str, id: int, task_id: Optional[str], station_id: str, participant_id: int,
                 timestamp_unix: str, timestamp_iso8601: str, confidence: float, norm_pos_x: float, norm_pos_y: float,
                 gaze_point_3d_x: float, gaze_point_3d_y: float, gaze_point_3d_z: float, eye_center0_3d_x: float,
                 eye_center0_3d_y: float, eye_center0_3d_z: float, eye_center1_3d_x: float, eye_center1_3d_y: float,
                 eye_center1_3d_z: float, gaze_normal0_x: float, gaze_normal0_y: float, gaze_normal0_z: float,
                 gaze_normal1_x: float, gaze_normal1_y: float, gaze_normal1_z: float, diameter0_2d: float,
                 diameter1_2d: float, diameter0_3d: float, diameter1_3d: float):
        super().__init__()

        self.group_session_id = group_session_id
        self.id = id
        self.task_id = task_id
        self.station_id = station_id
        self.participant_id = participant_id
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.confidence = confidence
        self.norm_pos_x = norm_pos_x
        self.norm_pos_y = norm_pos_y
        self.gaze_point_3d_x = gaze_point_3d_x
        self.gaze_point_3d_y = gaze_point_3d_y
        self.gaze_point_3d_z = gaze_point_3d_z
        self.eye_center0_3d_x = eye_center0_3d_x
        self.eye_center0_3d_y = eye_center0_3d_y
        self.eye_center0_3d_z = eye_center0_3d_z
        self.eye_center1_3d_x = eye_center1_3d_x
        self.eye_center1_3d_y = eye_center1_3d_y
        self.eye_center1_3d_z = eye_center1_3d_z
        self.gaze_normal0_x = gaze_normal0_x
        self.gaze_normal0_y = gaze_normal0_y
        self.gaze_normal0_z = gaze_normal0_z
        self.gaze_normal1_x = gaze_normal1_x
        self.gaze_normal1_y = gaze_normal1_y
        self.gaze_normal1_z = gaze_normal1_z
        self.diameter0_2d = diameter0_2d
        self.diameter1_2d = diameter1_2d
        self.diameter0_3d = diameter0_3d
        self.diameter1_3d = diameter1_3d

    @staticmethod
    def get_next_id(database_engine, group_session_id, station_id):
        with Session(database_engine) as session:
            max_id = session.query(func.max(GAZERaw.id)).filter_by(group_session_id=group_session_id,
                                                                    station_id=station_id).scalar()
            return max_id + 1 if max_id is not None else 1
