from typing import Optional

from network import send


def request_clients_end(to_client_connections, extra_data: Optional[dict] = None):
    data = {}
    data["type"] = "request"
    data["request"] = "end"

    if extra_data is not None:
        data.update(extra_data)

    send(to_client_connections, data)
