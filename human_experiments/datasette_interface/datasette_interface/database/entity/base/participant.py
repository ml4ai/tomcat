from sqlalchemy import Integer
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base


class Participant(Base):
    __tablename__ = "participant"

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
