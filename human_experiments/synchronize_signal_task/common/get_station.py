from sqlalchemy import text


def get_station(session: str, participant_id: int, task: str, engine) -> str:
    with engine.connect() as conn:
        query = text(
            f"""
            SELECT station
            FROM data_validity
            WHERE group_session = '{session}' AND participant = '{participant_id}' AND task = '{task}'
            """
        )
        station = conn.execute(query).fetchone()[0]

    return station
