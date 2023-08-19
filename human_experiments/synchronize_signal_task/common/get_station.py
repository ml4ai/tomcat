import sqlite3


def get_station(db_path: str, session: str, participant_id: int, task: str) -> str:
    db = sqlite3.connect(db_path)

    station = db.execute(f"""
        SELECT station
        FROM data_validity
        WHERE group_session = '{session}' AND participant = '{participant_id}' AND task = '{task}'
    """).fetchall()[0][0]

    return station
