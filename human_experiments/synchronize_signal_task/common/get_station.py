from sqlalchemy import create_engine, text

from config import POSTGRESQL_ENGINE


def get_station(session: str, participant_id: int, task: str) -> str:
    with create_engine(POSTGRESQL_ENGINE).connect() as conn:
        query = text(
            f"""
            SELECT station
            FROM data_validity
            WHERE group_session = '{session}' AND participant = '{participant_id}' AND task = '{task}'
            """
        )
        station = conn.execute(query).fetchone()[0]

    return station
