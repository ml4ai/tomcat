from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import Session, sessionmaker
from sqlalchemy_utils import database_exists, create_database

from dataset_pipeline.common.config import DEVELOPMENT, RUN_DIR, settings

DB_USER = settings.db_user
DB_PASS = settings.db_pass
DB_NAME = settings.db_name
DB_HOST = settings.db_host
DB_PORT = settings.db_port

if settings.working_env == DEVELOPMENT:
    SQLALCHEMY_DATABASE_URI = f"sqlite:///{RUN_DIR}/tomcat.db"
elif DB_HOST.startswith("/"):
    # DB_HOST is a Unix-socket directory (e.g. /var/run/postgresql) -> connect over the
    # local socket with peer authentication: no password is sent, and Postgres maps the
    # OS user to the PG role of the same name. Keeps secrets out of config entirely.
    SQLALCHEMY_DATABASE_URI = (
        f"postgresql://{DB_USER}@/{DB_NAME}?host={DB_HOST}&port={DB_PORT}"
    )
else:
    SQLALCHEMY_DATABASE_URI = (
        f"postgresql://{DB_USER}:{DB_PASS}@{DB_HOST}:{DB_PORT}/{DB_NAME}"
    )

Base = declarative_base()
engine = create_engine(SQLALCHEMY_DATABASE_URI)

# Create the database in Postgres if it doesn't already exist.
if not database_exists(engine.url):
    create_database(engine.url)

SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)


def get_db() -> Session:
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()
