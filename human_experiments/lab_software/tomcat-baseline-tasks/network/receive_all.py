from select import select
from time import time
from typing import Optional

from .utils import read_message


def receive_all(senders: dict, wait_time: Optional[float] = None) -> dict:
    """Wait until data from all channels are received or until specified wait time

    :param senders: sender channels
    :param wait_time: maximum time to block
    :return: key: channel name, value: data
    """
    data = {}

    # senders that have not sent data yet
    waiting_for_senders = list(senders.keys()).copy()

    # track how long to block
    start_time = time()

    # keep looping until data from all senders received or when timer runs out
    while waiting_for_senders:
        # get data ready to be read from channels
        senders_replied, _, exceptional = select(waiting_for_senders, [], waiting_for_senders, 0.01)

        if exceptional:
            raise RuntimeError("Connection lost")

        for connection in senders_replied:
            message = read_message(connection)
            if message is None:
                continue

            data[senders[connection]] = message

            waiting_for_senders.remove(connection)

        if wait_time is not None and time() - start_time > wait_time:
            break

    return data
