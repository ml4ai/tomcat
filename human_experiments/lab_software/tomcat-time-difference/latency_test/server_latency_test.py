import threading
from time import time

from network import receive, send


class ServerLatencyTest:
    def __init__(self, to_client_connections: dict, from_client_connections: dict) -> None:
        self._to_client_connections = to_client_connections
        self._from_client_connections = from_client_connections

        self._latencies = {}

    def run(self):
        data = {
            "type": "request",
            "request": "timestamp"
        }

        threads = []
        for from_client_connection, client_name in self._from_client_connections.items():
            to_client_connection = self._to_client_connections[client_name]
            latency_thread = threading.Thread(target=self._get_timestamp_difference, args=(data, client_name, to_client_connection, from_client_connection))
            threads.append(latency_thread)

        for latency_thread in threads:
            latency_thread.start()

        for latency_thread in threads:
            latency_thread.join()

        for client_name, time_difference in self._latencies.items():
            print(f"{client_name}: {time_difference}")

    def _get_timestamp_difference(self, data, client_name, to_client_connection, from_client_connection):
        send([to_client_connection], data)
        [data] = receive([from_client_connection])

        if data["type"] == "state":
            self._latencies[client_name] = time() - data["state"]
        else:
            raise RuntimeError("Cannot handle data of type: " + data["type"])
