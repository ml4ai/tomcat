from sqlalchemy import create_engine, Engine

DATA_MODES = ["raw", "sync", "filtered"]


class PostgresDB:
    """
    This class handles operations in a Postgres database.
    """

    def __init__(self,
                 db_name: str,
                 db_user: str,
                 db_host: str,
                 db_port: str,
                 db_passwd: str):
        """
        Creates an instance of the PostgresDB.

        :param db_name: name of the postgres database.
        :param db_user: user with reading privileges in the database.
        :param db_host: address of the database (e.g., localhost).
        :param db_port: port of the database (e.g., 5432).
        :param db_passwd: password to read from the database.
        """
        self.db_name = db_name
        self.db_user = db_user
        self.db_host = db_host
        self.db_port = db_port
        self.db_passwd = db_passwd

    def create_engine(self) -> Engine:
        """
        Creates a database engine.

        :return: a database engine.
        """
        database_info = f"{self.db_user}:{self.db_name}@{self.db_host}:{self.db_port}"
        connection_string = f"postgresql+psycopg2://{database_info}/{self.db_passwd}"
        return create_engine(connection_string)
