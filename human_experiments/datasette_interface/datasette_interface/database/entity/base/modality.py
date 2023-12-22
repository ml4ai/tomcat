from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import Text

from datasette_interface.database.entity.base.base import Base


class Modality(Base):
    __tablename__ = "modality"

    id: Mapped[str] = mapped_column(Text, primary_key=True)
