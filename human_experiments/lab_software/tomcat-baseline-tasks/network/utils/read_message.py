import json

from .config_network import HEADER


def read_message(connection):
    message = connection.recv(HEADER)

    try:
        message = json.loads(message.decode('utf-8'))
    except json.decoder.JSONDecodeError:
        print("[INFO] JSON failed to decode message")
        message = None

    return message
