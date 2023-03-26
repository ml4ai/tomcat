from typing import Optional

import argparse
import os
from multiprocessing import Process

from common import client_ai_teaming, pairing_clients
from common.lsl import LSLStringStream
from config import (DEFAULT_DATA_SAVE_PATH, DEFAULT_SERVER_ADDR,
                    DEFAULT_SERVER_PORT, DEFAULT_NUMBER_OF_HUMAN_SUBJECTS)
from network import Server, send
from tasks.affective_task import ServerAffectiveTask
from tasks.finger_tapping_task import ServerFingerTappingTask
from tasks.ping_pong_task import ServerPingPongTask
from tasks.rest_state import ServerRestState


def _send_start(to_client_connections: list):
    data = {}
    data["type"] = "request"
    data["request"] = "start"
    send(to_client_connections, data)


def _run_ping_pong(to_client_connections: list,
                   from_client_connections: dict,
                   session_name: str,
                   easy_mode: bool = True,
                   data_save_path: str = '',
                   lsl: Optional[LSLStringStream] = None):
    server_ping_pong_task = ServerPingPongTask(to_client_connections,
                                               from_client_connections,
                                               easy_mode=easy_mode,
                                               session_name=session_name,
                                               data_save_path=data_save_path,
                                               lsl=lsl)
    server_ping_pong_task.run()


def _run_affective_task(to_client_connections: list,
                        from_client_connections: dict,
                        session_name: str,
                        data_save_path: str = ''):
    server_affective_task = ServerAffectiveTask(to_client_connections,
                                                from_client_connections,
                                                session_name=session_name,
                                                data_save_path=data_save_path)

    server_affective_task.run("./tasks/affective_task/images/task_images", collaboration=False)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Run server of finger tapping task.')
    parser.add_argument("-a", "--address", default=DEFAULT_SERVER_ADDR, help="IP address of server")
    parser.add_argument("-p", "--port", type=int, default=DEFAULT_SERVER_PORT, help="Port of server")
    parser.add_argument("-s", "--save", default=DEFAULT_DATA_SAVE_PATH, help="Specify where to save data")
    parser.add_argument("-t", "--task", choices=["rest_state", "finger_tapping", "affective", "ping_pong"],
                        default="rest_state", help="The task we want to start from.")
    parser.add_argument("-g", "--group_size", type=int, choices=[2, 3, 4], default=DEFAULT_NUMBER_OF_HUMAN_SUBJECTS,
                        help="The number of subjects participating on the experiment.")
    args = parser.parse_args()

    data_path = args.save + "/rest_state"
    if not os.path.exists(data_path):
        os.makedirs(data_path)

    data_path = args.save + "/finger_tapping"
    if not os.path.exists(data_path):
        os.makedirs(data_path)

    data_path = args.save + "/affective"
    if not os.path.exists(data_path):
        os.makedirs(data_path)

    data_path = args.save + "/ping_pong"
    if not os.path.exists(data_path):
        os.makedirs(data_path)

    server = Server(args.address, args.port)
    num_participants = args.group_size

    # We need to start LSL Streams here so it becomes available on LabRecorder, otherwise it won't record the messages
    #  the individual tasks will broadcast. We pass this object down to the tasks so they can use it to send data
    # to LSL.
    # Also, to make parsing the .xdf for a specific task easier, I decided to create individual streams per task.
    rest_state_lsl_stream = LSLStringStream("Rest State", "rest_state")
    finger_tapping_lsl_stream = LSLStringStream("Finger Tapping", "finger_tapping")
    affective_task_lsl_stream = LSLStringStream("Affective Task", "affective_task")
    ping_pong_lsl_stream = LSLStringStream("Ping-Pong", "ping_pong")

    next_task = args.task

    # Initial rest state

    if next_task == "rest_state":
        print("")
        print("----------------------------------------------")
        print("                  REST STATE                  ")
        print("----------------------------------------------")
        print("")

        server.establish_connections(num_required_connections=num_participants)

        _send_start(list(server.to_client_connections.values()))

        server_rest_state = ServerRestState(list(server.to_client_connections.values()),
                                            server.from_client_connections,
                                            data_save_path=args.save,
                                            lsl=rest_state_lsl_stream)
        server_rest_state.run()

        next_task = "finger_tapping"

    # Finger tapping task

    if next_task == "finger_tapping":
        print("")
        print("----------------------------------------------")
        print("               FINGER TAPPING                 ")
        print("----------------------------------------------")
        print("")

        server.establish_connections(num_required_connections=num_participants)

        _send_start(list(server.to_client_connections.values()))

        server_finger_tapping_task = ServerFingerTappingTask(list(server.to_client_connections.values()),
                                                             server.from_client_connections,
                                                             data_save_path=args.save,
                                                             lsl=finger_tapping_lsl_stream)
        server_finger_tapping_task.run()

        next_task = "affective"

    # Individual affective task
    if next_task == "affective":
        print("")
        print("----------------------------------------------")
        print("               AFFECTIVE TASK                 ")
        print("----------------------------------------------")
        print("")

        server.establish_connections(num_required_connections=num_participants)

        _send_start(list(server.to_client_connections.values()))

        affective_task_processes = []
        for from_client_connection, client_name in server.from_client_connections.items():
            to_client_connection = server.to_client_connections[client_name]
            session_name = "individual_" + client_name
            process = Process(target=_run_affective_task,
                              args=(
                                  [to_client_connection], {from_client_connection: client_name}, session_name,
                                  args.save))
            affective_task_processes.append(process)

        for process in affective_task_processes:
            process.start()

        for process in affective_task_processes:
            process.join()

        # Team affective task

        server.establish_connections(num_required_connections=num_participants)

        _send_start(list(server.to_client_connections.values()))

        server_affective_task = ServerAffectiveTask(list(server.to_client_connections.values()),
                                                    server.from_client_connections,
                                                    session_name="team",
                                                    data_save_path=args.save,
                                                    lsl=affective_task_lsl_stream)

        server_affective_task.run("./tasks/affective_task/images/task_images", collaboration=True)

        server_affective_task.close_file()

        next_task = "ping_pong"

    # Ping pong competitive
    if next_task == "ping_pong":
        print("")
        print("----------------------------------------------")
        print("                  PING-PONG                   ")
        print("----------------------------------------------")
        print("")

        if num_participants % 2 != 0:
            # If the number of participants is even, we need one more connection which should be an extra player or AI.
            num_required_connections = num_participants + 1
        else:
            num_required_connections = num_participants

        server.establish_connections(num_required_connections=num_required_connections)

        _send_start(list(server.to_client_connections.values()))

        client_pairs = pairing_clients(server.to_client_connections, server.from_client_connections)

        ping_pong_processes = []
        for session_id, (to_client_connection_pair, from_client_connection_pair) in enumerate(client_pairs):
            to_client_connections = []
            for to_client_connection_team in to_client_connection_pair:
                to_client_connections = to_client_connections + list(to_client_connection_team.values())

            session_name = "competitive_" + str(session_id)
            process = Process(target=_run_ping_pong,
                              args=(to_client_connections, from_client_connection_pair, session_name, True, args.save,
                                    ping_pong_lsl_stream))
            ping_pong_processes.append(process)

        for process in ping_pong_processes:
            process.start()

        for process in ping_pong_processes:
            process.join()

        # Ping pong cooperative

        server.establish_connections(num_required_connections=num_required_connections)

        _send_start(list(server.to_client_connections.values()))

        client_pairs = client_ai_teaming(server.to_client_connections, server.from_client_connections)

        ping_pong_processes = []
        for session_id, (to_client_connection_teams, from_client_connection_teams) in enumerate(client_pairs):
            to_client_connections = []
            for to_client_connection_team in to_client_connection_teams:
                to_client_connections = to_client_connections + list(to_client_connection_team.values())

            session_name = "cooperative_" + str(session_id)
            process = Process(target=_run_ping_pong,
                              args=(
                                  to_client_connections, from_client_connection_teams, session_name, False, args.save))
            ping_pong_processes.append(process)

        for process in ping_pong_processes:
            process.start()

        for process in ping_pong_processes:
            process.join()

    print("")
    input("This is the end of the Baseline Tasks phase. Press Enter to wrap it up.")
    server.close_connections_listener()
