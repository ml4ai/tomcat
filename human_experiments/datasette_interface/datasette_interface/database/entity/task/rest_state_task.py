from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import ForeignKey
from sqlalchemy import Text

from datasette_interface.database.entity.base.base import Base


class RestStateTask(Base):
    __tablename__ = "rest_state_task"

    group_session_id: Mapped[str] = mapped_column("group_session", Text,
                                                  ForeignKey("group_session.id"), primary_key=True)
    start_timestamp_unix: Mapped[str] = mapped_column(Text)
    start_timestamp_iso8601: Mapped[str] = mapped_column(Text)
    stop_timestamp_unix: Mapped[str] = mapped_column(Text)
    stop_timestamp_iso8601: Mapped[str] = mapped_column(Text)
