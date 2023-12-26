from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import Text

from datasette_interface.database.config import Base


class Station(Base):
    __tablename__ = "station"

    id: Mapped[str] = mapped_column(Text, primary_key=True)
