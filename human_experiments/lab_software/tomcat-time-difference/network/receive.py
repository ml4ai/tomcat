from select import select
from typing import Optional, Union

from .utils import read_message


def receive(senders: Union[list, dict], wait_time: Optional[float] = None) -> Union[list, dict]:
    """Get data from channels

    :param senders: sender channels
    :param wait_time: maximum blocking time
    :return: data from channel, return type follows the data type of senders
        if senders is dictionary, then return key: channel names, value: data
    """
    if isinstance(senders, dict):
        return _receive_from_dict(senders, wait_time)
    else:
        return _receive_from_list(senders, wait_time)


def _receive_from_list(senders: list, wait_time: Optional[float] = None) -> list:
    senders, _, exceptional = select(senders, [], senders, wait_time)

    if exceptional:
        raise RuntimeError("Connection lost")

    data = []

    for connection in senders:
        message = read_message(connection)
        if message is None:
            continue

        data.append(message)

    return data


def _receive_from_dict(senders: dict, wait_time: Optional[float] = None) -> dict:
    senders_ready, _, exceptional = select(senders.keys(), [], senders.keys(), wait_time)

    if exceptional:
        raise RuntimeError("Connection lost")

    data = {}

    for connection in senders_ready:
        message = read_message(connection)
        if message is None:
            continue

        data[senders[connection]] = message

    return data
