import argparse
import os
from multiprocessing import Process

from common import client_ai_teaming, pairing_clients
from config import (DEFAULT_DATA_SAVE_PATH, DEFAULT_SERVER_ADDR,
                    DEFAULT_SERVER_PORT)
from network import Server, send
from tasks.affective_task import ServerAffectiveTask
from tasks.finger_tapping_task import ServerFingerTappingTask
from tasks.ping_pong_task import ServerPingPongTask
from tasks.rest_state import ServerRestState

REQUIRED_NUM_CONNECTIONS_REST_STATE = [1, 2, 3]
REQUIRED_NUM_CONNECTIONS_FINGER_TAPPING_TASK = [1, 2, 3]
REQUIRED_NUM_CONNECTIONS_AFFECTIVE_TASK = [1, 2, 3]
REQUIRED_NUM_CONNECTIONS_COMPETITIVE_PING_PONG_TASK = [2, 4]
REQUIRED_NUM_CONNECTIONS_COOPERATIVE_PING_PONG_TASK = [3, 4]


def _send_start(to_client_connections: list):
    data = {}
    data["type"] = "request"
    data["request"] = "start"
    send(to_client_connections, data)


def _run_ping_pong(to_client_connections: list,
                   from_client_connections: dict,
                   session_name: str,
                   easy_mode: bool = True,
                   data_save_path: str = ''):
    server_ping_pong_task = ServerPingPongTask(to_client_connections, 
                                               from_client_connections,
                                               easy_mode=easy_mode,
                                               session_name=session_name,
                                               data_save_path=data_save_path)
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
    args = parser.parse_args()

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

    # Initial rest state

    server.establish_connections(REQUIRED_NUM_CONNECTIONS_REST_STATE)

    _send_start(list(server.to_client_connections.values()))

    server_rest_state = ServerRestState(list(server.to_client_connections.values()), 
                                        server.from_client_connections)
    server_rest_state.run()

    # Finger tapping task

    server.establish_connections(REQUIRED_NUM_CONNECTIONS_FINGER_TAPPING_TASK)

    _send_start(list(server.to_client_connections.values()))

    server_finger_tapping_task = ServerFingerTappingTask(list(server.to_client_connections.values()), 
                                                         server.from_client_connections,
                                                         data_save_path=args.save)
    server_finger_tapping_task.run()

    # Individual affective task

    server.establish_connections(REQUIRED_NUM_CONNECTIONS_AFFECTIVE_TASK)

    _send_start(list(server.to_client_connections.values()))

    affective_task_processes = []
    for from_client_connection, client_name in server.from_client_connections.items():
        to_client_connection = server.to_client_connections[client_name]
        session_name = "individual_" + client_name
        process = Process(target=_run_affective_task, 
                          args=([to_client_connection], {from_client_connection: client_name}, session_name, args.save))
        affective_task_processes.append(process)

    for process in affective_task_processes:
        process.start()

    for process in affective_task_processes:
        process.join()

    # Team affective task

    server.establish_connections(REQUIRED_NUM_CONNECTIONS_AFFECTIVE_TASK)

    _send_start(list(server.to_client_connections.values()))

    server_affective_task = ServerAffectiveTask(list(server.to_client_connections.values()), 
                                                     server.from_client_connections,
                                                     session_name="team",
                                                     data_save_path=args.save)

    server_affective_task.run("./tasks/affective_task/images/task_images", collaboration=True)

    server_affective_task.close_file()

    # Ping pong competitive

    server.establish_connections(REQUIRED_NUM_CONNECTIONS_COMPETITIVE_PING_PONG_TASK)

    _send_start(list(server.to_client_connections.values()))

    client_pairs = pairing_clients(server.to_client_connections, server.from_client_connections)

    ping_pong_processes = []
    for session_id, (to_client_connection_pair, from_client_connection_pair) in enumerate(client_pairs):
        to_client_connections = []
        for to_client_connection_team in to_client_connection_pair:
            to_client_connections = to_client_connections + list(to_client_connection_team.values())

        session_name = "competitive_" + str(session_id)
        process = Process(target=_run_ping_pong, args=(to_client_connections, from_client_connection_pair, session_name, True, args.save))
        ping_pong_processes.append(process)

    for process in ping_pong_processes:
        process.start()

    for process in ping_pong_processes:
        process.join()

    # Ping pong cooperative

    server.establish_connections(REQUIRED_NUM_CONNECTIONS_COOPERATIVE_PING_PONG_TASK)

    _send_start(list(server.to_client_connections.values()))

    client_pairs = client_ai_teaming(server.to_client_connections, server.from_client_connections)

    ping_pong_processes = []
    for session_id, (to_client_connection_teams, from_client_connection_teams) in enumerate(client_pairs):
        to_client_connections = []
        for to_client_connection_team in to_client_connection_teams:
            to_client_connections = to_client_connections + list(to_client_connection_team.values())

        session_name = "cooperative_" + str(session_id)
        process = Process(target=_run_ping_pong, args=(to_client_connections, from_client_connection_teams, session_name, False, args.save))
        ping_pong_processes.append(process)

    for process in ping_pong_processes:
        process.start()

    for process in ping_pong_processes:
        process.join()

    server.establish_connections()
    server.close_connections_listener()
