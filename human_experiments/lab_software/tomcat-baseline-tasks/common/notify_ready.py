from network import send


def notify_ready(to_server):
    data = {}
    data["type"] = "status"
    data["status"] = "ready"
    send([to_server], data)
