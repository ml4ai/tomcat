from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import Text

from entity.base.base import Base


class AffectiveTaskEvent(Base):
    __tablename__ = "affective_task_event"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"),
                                                  primary_key=True, nullable=False)
    participant_id: Mapped[int] = mapped_column("participant", Integer, ForeignKey("participant.id"), primary_key=True,
                                                nullable=False)
    task_type: Mapped[str] = mapped_column(Text, primary_key=True, nullable=False)
    timestamp_unix: Mapped[str] = mapped_column(Text, primary_key=True, nullable=False)
    timestamp_iso8601: Mapped[str] = mapped_column(Text, nullable=False)
    event_type: Mapped[str] = mapped_column(Text, nullable=False)
    image_path: Mapped[str] = mapped_column(Text, )
    arousal_score: Mapped[int] = mapped_column(Integer)
    valence_score: Mapped[int] = mapped_column(Integer)

    def __init__(self, group_session_id: str, participant_id: int, task_type: str, timestamp_unix: str,
                 timestamp_iso8601: str, event_type: str, image_path: str, arousal_score: int, valence_score: int):
        super().__init__()

        self.group_session_id = group_session_id
        self.participant_id = participant_id
        self.task_type = task_type
        self.timestamp_unix = timestamp_unix
        self.timestamp_iso8601 = timestamp_iso8601
        self.event_type = event_type
        self.image_path = image_path
        self.arousal_score = arousal_score
        self.valence_score = valence_score


        # CREATE TABLE affective_task_event (
        group_session
        TEXT
        NOT
        NULL,
        participant
        INTEGER,
        task_type
        TEXT
        NOT
        NULL,
        timestamp_unix
        TEXT
        NOT
        NULL,
        timestamp_iso8601
        TEXT
        NOT
        NULL,
        event_type
        TEXT
        NOT
        NULL,
        image_path
        TEXT,
        arousal_score
        INTEGER,
        valence_score
        INTEGER,
        FOREIGN
        KEY(group_session)
        REFERENCES
        group_session(id)
        FOREIGN
        KEY(participant)
        REFERENCES
        participant(id)

    );"""
    #
    #
