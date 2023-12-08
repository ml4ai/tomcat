import os

from sqlalchemy import create_engine

DB_PASSWD = os.getenv("DB_PASSWD", "")
DEFAULT_USER = os.getenv("DB_USER", os.getenv("USER"))
DEFAULT_HOST = os.getenv("DB_HOST", "localhost")
DEFAULT_PORT = os.getenv("DB_PORT", 5433)
DEFAULT_DB_NAME = os.getenv("DB_NAME", "tomcat")
DEFAULT_CONNECTION_STRING = f"{DEFAULT_USER}:{DB_PASSWD}@{DEFAULT_HOST}:" \
                            f"{DEFAULT_PORT}/{DEFAULT_DB_NAME}"
TOMCAT_DATABASE_ENGINE = create_engine("postgresql+psycopg2://" + DEFAULT_CONNECTION_STRING)
