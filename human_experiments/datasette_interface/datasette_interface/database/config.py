from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import Session, sessionmaker

from datasette_interface.common.config import DEVELOPMENT, RUN_DIR, settings

DB_USER = settings.db_user
DB_PASS = settings.db_pass
DB_NAME = settings.db_name
DB_HOST = settings.db_host
DB_PORT = settings.db_port

if settings.working_env == DEVELOPMENT:
    SQLALCHEMY_DATABASE_URI = f"sqlite:///{RUN_DIR}/tomcat.db"
else:
    SQLALCHEMY_DATABASE_URI = (
        f"postgresql://{DB_USER}:{DB_PASS}@{DB_HOST}:{DB_PORT}/{DB_NAME}"
    )

Base = declarative_base()
engine = create_engine(SQLALCHEMY_DATABASE_URI)
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)


def get_db() -> Session:
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()
