from sqlalchemy import Integer, Text, Boolean
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base


class Participant(Base):
    __tablename__ = "participant"

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    age: Mapped[int] = mapped_column(Integer, nullable=True)
    sex: Mapped[str] = mapped_column(Text, nullable=True)
    hisp: Mapped[bool] = mapped_column(Boolean, nullable=True)
    race: Mapped[str] = mapped_column(Text, nullable=True)
    income: Mapped[str] = mapped_column(Text, nullable=True)
    edu: Mapped[str] = mapped_column(Text, nullable=True)
    exp: Mapped[str] = mapped_column(Text, nullable=True)
    exp_mc: Mapped[str] = mapped_column(Text, nullable=True)
    handedness: Mapped[str] = mapped_column(Text, nullable=True)
    trackpad_preference: Mapped[str] = mapped_column(Text, nullable=True)
    sph_label: Mapped[str] = mapped_column(Text, nullable=True)
    shl_impairments: Mapped[str] = mapped_column(Text, nullable=True)
    shl_impairments_specify: Mapped[str] = mapped_column(Text, nullable=True)
    shl_impairments_agediagnosis: Mapped[str] = mapped_column(
        Text, nullable=True
    )
    shl_impairments_therapy: Mapped[str] = mapped_column(Text, nullable=True)
    first_language: Mapped[str] = mapped_column(Text, nullable=True)
    languages_spoken: Mapped[str] = mapped_column(Text, nullable=True)
    language_age_learned: Mapped[str] = mapped_column(Text, nullable=True)
    countries_live_one_year: Mapped[str] = mapped_column(Text, nullable=True)
    major_schooling_country: Mapped[str] = mapped_column(Text, nullable=True)
    health_label: Mapped[str] = mapped_column(Text, nullable=True)
    health_concussion: Mapped[str] = mapped_column(Text, nullable=True)
    health_seizure: Mapped[str] = mapped_column(Text, nullable=True)
    health_trauma: Mapped[str] = mapped_column(Text, nullable=True)
    health_other_trauma_specify: Mapped[str] = mapped_column(
        Text, nullable=True
    )
    health_medications: Mapped[str] = mapped_column(Text, nullable=True)
    health_vision: Mapped[str] = mapped_column(Text, nullable=True)
    health_vision_specify: Mapped[str] = mapped_column(Text, nullable=True)
