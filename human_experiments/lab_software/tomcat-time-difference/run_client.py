import argparse

from common import wait_for_server
from config import DEFAULT_SERVER_ADDR, DEFAULT_SERVER_PORT
from latency_test import ClientLatencyTest
from network import Client

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Run client of finger tapping task.')
    parser.add_argument("-a", "--address", default=DEFAULT_SERVER_ADDR, help="IP address of server")
    parser.add_argument("-p", "--port", type=int, default=DEFAULT_SERVER_PORT, help="Port of server")
    parser.add_argument("-n", "--name", required=True, help="Name of client")
    args = parser.parse_args()

    client = Client(args.address, args.port, args.name)

    wait_for_server(client.to_server, client.from_server)

    client_latency_test= ClientLatencyTest(client.from_server, 
                                           client.to_server)
    client_latency_test.run()

    client.close()
