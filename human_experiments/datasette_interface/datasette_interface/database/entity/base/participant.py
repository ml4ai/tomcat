from typing import Optional
from sqlalchemy import Integer, Text, Boolean
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base


class Participant(Base):
    __tablename__ = "participant"

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    age: Mapped[Optional[int]] = mapped_column(Integer)
    sex: Mapped[Optional[str]] = mapped_column(Text)
    hisp: Mapped[Optional[bool]] = mapped_column(Boolean)
    race: Mapped[Optional[str]] = mapped_column(Text)
    income: Mapped[Optional[str]] = mapped_column(Text)
    edu: Mapped[Optional[str]] = mapped_column(Text)
    exp: Mapped[Optional[str]] = mapped_column(Text)
    exp_mc: Mapped[Optional[str]] = mapped_column(Text)
    handedness: Mapped[Optional[str]] = mapped_column(Text)
    trackpad_preference: Mapped[Optional[str]] = mapped_column(Text)
    sph_label: Mapped[Optional[str]] = mapped_column(Text)
    shl_impairments: Mapped[Optional[str]] = mapped_column(Text)
    shl_impairments_specify: Mapped[Optional[str]] = mapped_column(Text)
    shl_impairments_agediagnosis: Mapped[Optional[str]] = mapped_column(
        Text
    )
    shl_impairments_therapy: Mapped[Optional[str]] = mapped_column(Text)
    first_language: Mapped[Optional[str]] = mapped_column(Text)
    languages_spoken: Mapped[Optional[str]] = mapped_column(Text)
    language_age_learned: Mapped[Optional[str]] = mapped_column(Text)
    countries_live_one_year: Mapped[Optional[str]] = mapped_column(Text)
    major_schooling_country: Mapped[Optional[str]] = mapped_column(Text)
    health_label: Mapped[Optional[str]] = mapped_column(Text)
    health_concussion: Mapped[Optional[str]] = mapped_column(Text)
    health_seizure: Mapped[Optional[str]] = mapped_column(Text)
    health_trauma: Mapped[Optional[str]] = mapped_column(Text)
    health_other_trauma_specify: Mapped[Optional[str]] = mapped_column(
        Text
    )
    health_medications: Mapped[Optional[str]] = mapped_column(Text)
    health_vision: Mapped[Optional[str]] = mapped_column(Text)
    health_vision_specify: Mapped[Optional[str]] = mapped_column(Text)
