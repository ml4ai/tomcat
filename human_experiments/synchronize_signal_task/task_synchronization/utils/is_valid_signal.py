from sqlalchemy import text
import logging


def is_valid_signal(session: str, station: str, task: str, modality: str, engine) -> bool:
    with engine.connect() as conn:
        query = text(
            f"""
            SELECT is_valid
            FROM data_validity
            WHERE group_session = '{session}' AND station = '{station}' AND task = '{task}' AND modality = '{modality}'
            """
        )
        is_valid = conn.execute(query).fetchone()

        if is_valid is None:
            logging.warning(f"Cannot find {session} {station} {task} {modality}")
            return False

        is_valid = is_valid[0]
        assert isinstance(is_valid, int)

    return bool(is_valid)
