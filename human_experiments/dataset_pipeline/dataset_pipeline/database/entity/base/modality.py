from sqlalchemy import Text
from sqlalchemy.orm import Mapped, mapped_column

from dataset_pipeline.database.config import Base


class Modality(Base):
    __tablename__ = "modality"

    id: Mapped[str] = mapped_column(Text, primary_key=True)
