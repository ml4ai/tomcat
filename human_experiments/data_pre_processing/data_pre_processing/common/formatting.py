from pytz import timezone
from datetime import datetime

MST = timezone("US/Arizona")


def convert_unix_timestamp_to_iso8601(unix_timestamp):
    """
    Converts a timestamp in unix from MST time zone to ISO8601 in UTC.

    :param unix_timestamp: unix timestamp.
    :return: ISO8601 timestamp.
    """
    iso8601_timestamp = datetime.fromtimestamp(
        float(unix_timestamp), tz=MST
    ).astimezone(tz=timezone("UTC")).isoformat(timespec='microseconds')

    return iso8601_timestamp
