from time import sleep

from common import request_clients_end
from network import receive, send

from .config_rest_state import REST_TIMER


class ServerRestState:
    def __init__(self, to_client_connections: list, from_client_connections: dict) -> None:
        self._to_client_connections = to_client_connections
        self._from_client_connections = from_client_connections

    def run(self):
        data = {}
        data["type"] = "state"
        data["state"] = {}
        data["state"]["rest_timer"] = REST_TIMER

        print("[STATUS] Running rest state")
        send(self._to_client_connections, data)
        
        sleep(0.1)

        while(True):
            responses = receive(self._from_client_connections)
            response = list(responses.values())[0]
            if response["type"] == "STOP":
                request_clients_end(self._to_client_connections)
                print("[STATUS] Rest state has ended")
                break
            else:
                print("[ERROR] Rest state clients didn't send STOP message for Rest state server to terminate")

