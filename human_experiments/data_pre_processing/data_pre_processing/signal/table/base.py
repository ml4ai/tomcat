from sqlalchemy.orm import DeclarativeBase

from sqlalchemy import Text
from sqlalchemy.orm import Mapped, mapped_column


class Base(DeclarativeBase):
    """
    Base table.
    """
    pass


class GroupSession(Base):
    __tablename__ = "group_session"

    id: Mapped[str] = mapped_column(Text, primary_key=True)

    def __init__(self, id: str):
        super().__init__()
        self.id = id


class Station(Base):
    __tablename__ = "station"

    id: Mapped[str] = mapped_column(Text, primary_key=True)

    def __init__(self, id: str):
        super().__init__()
        self.id = id
