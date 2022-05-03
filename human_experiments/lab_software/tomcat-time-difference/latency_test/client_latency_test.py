from time import time

from network import receive, send


class ClientLatencyTest:
    def __init__(self, from_server, to_server):
        self._from_server = from_server
        self._to_server = to_server

    def run(self):
        [data] = receive([self._from_server])

        if data["type"] == "request" and data["request"] == "timestamp":
            response = {
                "type": "state",
                "state": time()
            }
            send([self._to_server], response)
