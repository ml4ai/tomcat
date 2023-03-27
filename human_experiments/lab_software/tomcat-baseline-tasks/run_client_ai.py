import argparse

import pygame

from common import wait_for_server
from config import DEFAULT_SERVER_ADDR, DEFAULT_SERVER_PORT
from network import Client
from tasks.ping_pong_task import ClientAIPingPongTask

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Run client of finger tapping task.')
    parser.add_argument("-a", "--address", default=DEFAULT_SERVER_ADDR, help="IP address of server")
    parser.add_argument("-p", "--port", type=int, default=DEFAULT_SERVER_PORT, help="Port of server")
    parser.add_argument("-n", "--name", default="ai", metavar='', help="Name of client")
    args = parser.parse_args()

    assert "ai" in args.name

    pygame.init()

    client = Client(args.address, args.port, args.name, "ai")

    wait_for_server(client.to_server, client.from_server)

    client_ai_ping_pong_task = ClientAIPingPongTask(client.from_server, 
                                                    client.to_server,
                                                    client.client_name,
                                                    easy_mode=False)
    client_ai_ping_pong_task.run()

    client.close()
