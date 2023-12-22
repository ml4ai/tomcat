from typing import Optional

from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import Text

from datasette_interface.database.entity.base.base import Base


class AffectiveTaskEvent(Base):
    __tablename__ = "affective_task_event"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"),
                                                  primary_key=True)
    participant_id: Mapped[int] = mapped_column("participant", Integer, ForeignKey("participant.id"), primary_key=True)
    task_type: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_unix: Mapped[str] = mapped_column(Text, primary_key=True)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    event_type: Mapped[str] = mapped_column(Text)
    image_path: Mapped[Optional[str]] = mapped_column(Text)
    arousal_score: Mapped[Optional[int]] = mapped_column(Integer)
    valence_score: Mapped[Optional[int]] = mapped_column(Integer)
