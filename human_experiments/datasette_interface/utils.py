import os
import sys
import contextlib
import logging
from logging import info
from config import logging_handlers
import dateutil
import datetime
from pytz import timezone
import time

MST = timezone("US/Arizona")


@contextlib.contextmanager
def cd(path):
    old_path = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(old_path)


def should_ignore_directory(session) -> bool:
    """Returns true if this directory should be ignored."""
    year, month, day, hour = [int(x) for x in session.split("_")[1:]]

    if year == 2022 and ((month < 9) or (month == 9 and day < 30)):
        info(
            f"Ignoring {session} since our first pilot with real "
            "participants was on 9/30/2022"
        )
        return True
    elif session == "exp_2023_04_20_14":
        info(
            f"Ignoring {session}. Since only one participant showed up, the session was cancelled."
        )
        return True

    elif session == "exp_2023_02_20_13":
        info(
            f"Ignoring {session}, since it is a duplicate of the "
            "exp_2023_02_20_01 directory."
        )
        return True

    elif session in {"exp_2022_12_05_15", "exp_2023_04_26_10"}:
        info(
            f"Ignoring {session}, since it was cancelled (no participants showed up.)"
        )
        return True
    else:
        return False


def is_directory_with_unified_xdf_files(session):
    year, month, day, hour = [int(x) for x in session.split("_")[1:]]
    return (year, month) >= (2023, 4)


def convert_unix_timestamp_to_iso8601(unix_timestamp):
    iso8601_timestamp = datetime.datetime.fromtimestamp(
        float(unix_timestamp), tz=MST
    ).astimezone(tz=datetime.timezone.utc).isoformat(timespec='microseconds')
    return iso8601_timestamp


def convert_iso8601_timestamp_to_unix(iso8601_timestamp):
    d = dateutil.parser.parse(iso8601_timestamp)
    unix_timestamp = d.timestamp()
    return unix_timestamp
