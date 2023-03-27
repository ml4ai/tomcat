import argparse

import pygame

from common import cursor_visibility, render_blank_screen
from config import (BLANK_SCREEN_COUNT_DOWN_MILLISECONDS, DEFAULT_SERVER_ADDR,
                    DEFAULT_SERVER_PORT, TASK_LIST)
from instructions import (affective_task_instruction_individual,
                          affective_task_instruction_team, exit_instruction,
                          finger_tapping_task_instruction,
                          introduction_instruction,
                          ping_pong_task_competitive_instruction,
                          ping_pong_task_cooperative_instruction,
                          wait_for_experimenter)
from network import Client
from tasks.affective_task import ClientAffectiveTask
from tasks.finger_tapping_task import ClientFingerTappingTask
from tasks.ping_pong_task import ClientPingPongTask
from tasks.rest_state import ClientRestState

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Run client of finger tapping task.')
    parser.add_argument("-a", "--address", default=DEFAULT_SERVER_ADDR, help="IP address of server")
    parser.add_argument("-p", "--port", type=int, default=DEFAULT_SERVER_PORT, help="Port of server")
    parser.add_argument("-n", "--name", required=True, help="Name of client")
    parser.add_argument("-i", "--id", required=True, help="Client's unique ID")
    parser.add_argument("-t", "--task", choices=TASK_LIST,
                        default="rest_state", help="The task we want to start from.")
    args = parser.parse_args()

    pygame.init()

    cursor_visibility(False)

    client = Client(args.address, args.port, args.name, args.id)

    screen = pygame.display.set_mode((0, 0), pygame.FULLSCREEN)

    tasks = TASK_LIST.copy()

    while args.task != tasks[0]:
        tasks.pop(0)

    # Initial rest state

    if tasks[0] == "rest_state":

        wait_for_experimenter(client.to_server, client.from_server, screen)

        client_rest_state = ClientRestState(client.from_server,
                                            client.to_server,
                                            screen)
        client_rest_state.run()

        render_blank_screen(screen, BLANK_SCREEN_COUNT_DOWN_MILLISECONDS)

        tasks.pop(0)
    
    # Introduction slides

    # introduction_instruction(screen)
    
    # Finger tapping task

    if tasks[0] == "finger_tapping":

        finger_tapping_task_instruction(screen)

        wait_for_experimenter(client.to_server, client.from_server, screen)

        client_finger_tapping_task = ClientFingerTappingTask(client.from_server,
                                                             client.to_server,
                                                             screen,
                                                             client.client_name)
        client_finger_tapping_task.run()

        render_blank_screen(screen, BLANK_SCREEN_COUNT_DOWN_MILLISECONDS)

        tasks.pop(0)

    if tasks[0] == "affective":
        # Individual
        affective_task_instruction_individual(screen)

        wait_for_experimenter(client.to_server, client.from_server, screen)

        client_affective_task = ClientAffectiveTask(client.from_server,
                                                    client.to_server,
                                                    screen)

        client_affective_task.run()

        render_blank_screen(screen, BLANK_SCREEN_COUNT_DOWN_MILLISECONDS)

        # Team
        affective_task_instruction_team(screen)

        wait_for_experimenter(client.to_server, client.from_server, screen)

        client_affective_task = ClientAffectiveTask(client.from_server,
                                                    client.to_server,
                                                    screen)

        client_affective_task.run(collaboration=True)

        render_blank_screen(screen, BLANK_SCREEN_COUNT_DOWN_MILLISECONDS)

        tasks.pop(0)

    if tasks[0] == "ping_pong":

        # Ping pong competitive task
        ping_pong_task_competitive_instruction(screen)

        wait_for_experimenter(client.to_server, client.from_server, screen)

        client_ping_pong_task = ClientPingPongTask(client.from_server,
                                                   client.to_server,
                                                   screen,
                                                   client.client_name)
        client_ping_pong_task.run()

        render_blank_screen(screen, BLANK_SCREEN_COUNT_DOWN_MILLISECONDS)

        # Ping pong cooperative task

        ping_pong_task_cooperative_instruction(screen)

        wait_for_experimenter(client.to_server, client.from_server, screen)

        client_ping_pong_task = ClientPingPongTask(client.from_server,
                                                   client.to_server,
                                                   screen,
                                                   client.client_name,
                                                   easy_mode=False)
        client_ping_pong_task.run()

        render_blank_screen(screen, BLANK_SCREEN_COUNT_DOWN_MILLISECONDS)

        tasks.pop(0)

    exit_instruction(client.to_server, screen)

    client.close()
