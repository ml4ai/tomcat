from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Text

from entity.base.base import Base


class RestStateTask(Base):
    __tablename__ = "rest_state_task"

    group_session_id: Mapped[str] = mapped_column("group_session", Text, ForeignKey("group_session.id"), primary_key=True)
    start_timestamp_unix: Mapped[str] = mapped_column(Text)
    start_timestamp_iso8601: Mapped[str] = mapped_column(Text)
    stop_timestamp_unix: Mapped[str] = mapped_column(Text)
    stop_timestamp_iso8601: Mapped[str] = mapped_column(Text)

    def __init__(self, group_session_id: str, start_timestamp_unix: str, start_timestamp_iso8601: str, stop_timestamp_unix: str, stop_timestamp_iso8601: str):
        super().__init__()

        self.group_session_id = group_session_id
        self.start_timestamp_unix = start_timestamp_unix
        self.start_timestamp_iso8601 = start_timestamp_iso8601
        self.stop_timestamp_unix = stop_timestamp_unix
        self.stop_timestamp_iso8601 = stop_timestamp_iso8601
