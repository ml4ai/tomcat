from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import Integer

from entity.base import Base


class Participant(Base):
    __tablename__ = "participant"

    id: Mapped[int] = mapped_column(Integer, primary_key=True)

    def __init__(self, id: int):
        super().__init__()
        self.id = id

