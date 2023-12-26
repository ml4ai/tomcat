from sqlalchemy import Text
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base


class Task(Base):
    __tablename__ = "task"

    id: Mapped[str] = mapped_column(Text, primary_key=True)
