from typing import Optional

from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import Text

from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.config import Base


class FingerTappingTaskObservation(Base):
    __tablename__ = "finger_tapping_task_observation"

    group_session_id: Mapped[str] = mapped_column("group_session", Text,
                                                  ForeignKey(GroupSession.id),
                                                  primary_key=True)
    timestamp_unix: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    event_type: Mapped[str] = mapped_column(Text)
    countdown_timer: Mapped[Optional[int]] = mapped_column(Integer)
    lion_spacebar_pressed: Mapped[Optional[int]] = mapped_column(Integer)
    tiger_spacebar_pressed: Mapped[Optional[int]] = mapped_column(Integer)
    leopard_spacebar_pressed: Mapped[Optional[int]] = mapped_column(Integer)
