from sqlalchemy import Text
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base


class Station(Base):
    __tablename__ = "station"

    id: Mapped[str] = mapped_column(Text, primary_key=True)
