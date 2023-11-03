import datetime
from pytz import timezone

MST = timezone("US/Arizona")


def convert_unix_timestamp_to_iso8601(unix_timestamp):
    iso8601_timestamp = datetime.datetime.fromtimestamp(
        float(unix_timestamp), tz=MST
    ).astimezone(tz=datetime.timezone.utc).isoformat(timespec='microseconds')
    return iso8601_timestamp
