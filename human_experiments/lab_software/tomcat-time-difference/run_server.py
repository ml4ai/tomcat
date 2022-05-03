import argparse

from config import DEFAULT_SERVER_ADDR, DEFAULT_SERVER_PORT
from latency_test import ServerLatencyTest
from network import Server, send

REQUIRED_NUM_CONNECTIONS = [1, 2, 3, 4, 5, 6]


def _send_start(to_client_connections: list):
    data = {}
    data["type"] = "request"
    data["request"] = "start"
    send(to_client_connections, data)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run time difference software.")
    parser.add_argument("-a", "--address", default=DEFAULT_SERVER_ADDR, help="IP address of server")
    parser.add_argument("-p", "--port", type=int, default=DEFAULT_SERVER_PORT, help="Port of server")
    args = parser.parse_args()

    server = Server(args.address, args.port)

    server.establish_connections(REQUIRED_NUM_CONNECTIONS)

    _send_start(list(server.to_client_connections.values()))

    server_latency_test = ServerLatencyTest(server.to_client_connections, 
                                            server.from_client_connections)
    server_latency_test.run()

    server.establish_connections()
    server.close_connections_listener()
