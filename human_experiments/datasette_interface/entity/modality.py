from sqlalchemy.orm import Mapped
from sqlalchemy.orm import mapped_column
from sqlalchemy import Text

from entity.base import Base


class Modality(Base):
    __tablename__ = "modality"

    id: Mapped[str] = mapped_column(Text, primary_key=True)

    def __init__(self, id: str):
        super().__init__()
        self.id = id
