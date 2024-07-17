from sqlalchemy import Integer
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base


class Participant(Base):
    __tablename__ = "participant"

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    age: Mapped[int] = mapped_column(Integer)
    sex: Mapped[str] = mapped_column(Text)
    hisp: Mapped[bool] = mapped_column(Boolean)
    race: Mapped[str] = mapped_column(Text)
    income: Mapped[str] = mapped_column(Text)
    edu: Mapped[str] = mapped_column(Text)
    exp: Mapped[str] = mapped_column(Text)
    exp_mc: Mapped[str] = mapped_column(Text)
    handedness: Mapped[str] = mapped_column(Text)
    trackpad_preference: Mapped[str] = mapped_column(Text)
    sph_label: Mapped[str] = mapped_column(Text)
    shl_impairements: Mapped[str] = mapped_column(Text) # Typo :(
    shl_impairments_specify: Mapped[str] = mapped_column(Text)
    shl_impairments_agediagnosis: Mapped[str] = mapped_column(Text)
    shl_impairments_therapy: Mapped[str] = mapped_column(Text)
    first_language: Mapped[str] = mapped_column(Text)
    languages_spoken: Mapped[str] = mapped_column(Text)
    language_age_learned: Mapped[str] = mapped_column(Text)
    countries_live_one_year: Mapped[str] = mapped_column(Text)
    major_schooling_country: Mapped[str] = mapped_column(Text)
    health_label: Mapped[str] = mapped_column(Text)
    health_concussion: Mapped[str] = mapped_column(Text)
    health_seizure: Mapped[str] = mapped_column(Text)
    health_trauma: Mapped[str] = mapped_column(Text)
    health_other_trauma_specify: Mapped[str] = mapped_column(Text)
    health_medications: Mapped[str] = mapped_column(Text)
    health_vision: Mapped[str] = mapped_column(Text)
    health_vision_specify: Mapped[str] = mapped_column(Text)
