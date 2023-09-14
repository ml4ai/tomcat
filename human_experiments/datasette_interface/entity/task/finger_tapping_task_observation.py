from typing import Optional

from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import Text

from entity.base.base import Base


class FingerTappingTaskObservation(Base):
    __tablename__ = "finger_tapping_task_observation"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"),
                                                  primary_key=True)
    timestamp_unix: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    event_type: Mapped[str] = mapped_column(Text)
    countdown_timer: Mapped[Optional[int]] = mapped_column(Integer)
    lion_spacebar_pressed: Mapped[Optional[int]] = mapped_column(Integer)
    tiger_spacebar_pressed: Mapped[Optional[int]] = mapped_column(Integer)
    leopard_spacebar_pressed: Mapped[Optional[int]] = mapped_column(Integer)

    def __init__(self, group_session_id: str, timestamp_unix: str, timestamp_iso8601: str, event_type: str,
                 countdown_timer: int, lion_spacebar_pressed: int, tiger_spacebar_pressed: int,
                 leopard_spacebar_pressed: int):
        super().__init__()

        self.group_session_id = group_session_id
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.event_type = event_type
        self.countdown_timer = countdown_timer
        self.lion_spacebar_pressed = lion_spacebar_pressed
        self.tiger_spacebar_pressed = tiger_spacebar_pressed
        self.leopard_spacebar_pressed = leopard_spacebar_pressed
