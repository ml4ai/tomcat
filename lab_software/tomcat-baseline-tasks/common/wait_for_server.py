from network import receive

from common import notify_ready


def wait_for_server(to_server, from_server):
    notify_ready(to_server)

    [data] = receive([from_server])
    if not (data["type"] == "request" and data["request"] == "start"):
        raise RuntimeError("Cannot parse request from server")
