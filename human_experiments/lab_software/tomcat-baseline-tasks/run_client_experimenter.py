import argparse

import pygame

from common import render_blank_screen
from config import (BLANK_SCREEN_COUNT_DOWN_MILLISECONDS, DEFAULT_SERVER_ADDR,
                    DEFAULT_SERVER_PORT)
from instructions import (ping_pong_task_competitive_instruction,
                          wait_for_experimenter)
from network import Client
from tasks.ping_pong_task import ClientPingPongTask

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Run client of finger tapping task.')
    parser.add_argument("-a", "--address", default=DEFAULT_SERVER_ADDR, help="IP address of server")
    parser.add_argument("-p", "--port", type=int, default=DEFAULT_SERVER_PORT, help="Port of server")
    parser.add_argument("-n", "--name", required=True, help="Name of client")
    args = parser.parse_args()

    pygame.init()
    pygame.mouse.set_visible(False)

    client = Client(args.address, args.port, args.name)

    screen = pygame.display.set_mode((0, 0), pygame.FULLSCREEN)

    ping_pong_task_competitive_instruction(screen)

    wait_for_experimenter(client.to_server, client.from_server, screen)

    render_blank_screen(screen, BLANK_SCREEN_COUNT_DOWN_MILLISECONDS)

    client_ping_pong_task = ClientPingPongTask(client.from_server, 
                                               client.to_server, 
                                               screen, 
                                               client.client_name)
    client_ping_pong_task.run()

    client.close()
