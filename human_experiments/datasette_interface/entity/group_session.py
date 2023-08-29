from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import Text

from entity.base import Base


class GroupSession(Base):
    __tablename__ = "group_session"

    id: Mapped[str] = mapped_column(Text, primary_key=True)
