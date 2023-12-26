from sqlalchemy import ForeignKey, Text
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base
from datasette_interface.database.entity.base.group_session import GroupSession


class RestStateTask(Base):
    __tablename__ = "rest_state_task"

    group_session_id: Mapped[str] = mapped_column(
        "group_session", Text, ForeignKey(GroupSession.id), primary_key=True
    )
    start_timestamp_unix: Mapped[str] = mapped_column(Text)
    start_timestamp_iso8601: Mapped[str] = mapped_column(Text)
    stop_timestamp_unix: Mapped[str] = mapped_column(Text)
    stop_timestamp_iso8601: Mapped[str] = mapped_column(Text)
