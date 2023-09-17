from sqlalchemy import create_engine, text

from config import POSTGRESQL_ENGINE


def is_valid_signal(session: str, station: str, task: str, modality: str) -> bool:
    with create_engine(POSTGRESQL_ENGINE).connect() as conn:
        query = text(
            f"""
            SELECT is_valid
            FROM data_validity
            WHERE group_session = '{session}' AND station = '{station}' AND task = '{task}' AND modality = '{modality}'
            """
        )
        is_valid = conn.execute(query).fetchone()[0]
        assert isinstance(is_valid, int)

    return bool(is_valid)
