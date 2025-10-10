from sqlalchemy import Text
from sqlalchemy.orm import Mapped, mapped_column

from datasette_interface.database.config import Base


class GroupSession(Base):
    __tablename__ = "group_session"

    id: Mapped[str] = mapped_column(Text, primary_key=True)
    advisor: Mapped[str] = mapped_column(Text)
    lion_minecraft_playername: Mapped[str] = mapped_column(Text, nullable=True)
    tiger_minecraft_playername: Mapped[str] = mapped_column(Text, nullable=True)
    leopard_minecraft_playername: Mapped[str] = mapped_column(Text, nullable=True)
