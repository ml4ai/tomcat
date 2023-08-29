from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import Text

from entity.base import Base


class Task(Base):
    __tablename__ = "task"

    id: Mapped[str] = mapped_column(Text, primary_key=True)
