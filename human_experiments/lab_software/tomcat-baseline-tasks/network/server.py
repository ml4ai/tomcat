import socket
import sys
import threading
from select import select

from .receive import receive
from .send import send
from .client import ClientPayload


class Server:
    """Establish connection with client channels and handle requests
    """

    def __init__(self, host: str, port: int) -> None:
        # Establish connection where clients can get game state update
        self._to_client_request = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._to_client_request.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)  # Reuse socket
        self._to_client_request.bind((host, port))
        self._to_client_request.setblocking(False)

        # Establish connection where clients send control commands
        self._from_client_request = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._from_client_request.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)  # Reuse socket
        self._from_client_request.bind((host, port + 1))
        self._from_client_request.setblocking(False)

        print(f"Address: {host}, {port}")

        self.to_client_connections = {}  # Key: client name, Value: client connection
        self.from_client_connections = {}  # Key: client connection, Value: client name

        self._establishing_connections = False
        self._num_required_connections = 0

        self._client_name_to_id = {}

    def get_client_id(self, name: str) -> str:
        return self._client_name_to_id.get(name, "")

    def establish_connections(self, num_required_connections: int) -> None:
        """Open to establishing new connections
        """
        self._establishing_connections = True

        self._num_required_connections = num_required_connections

        to_client_request_thread = threading.Thread(target=self._dispatch_to_client_request, daemon=True)
        to_client_request_thread.start()

        from_client_request_thread = threading.Thread(target=self._dispatch_from_client_request, daemon=True)
        from_client_request_thread.start()

        from_clients_thread = threading.Thread(target=self._from_clients, daemon=True)
        from_clients_thread.start()

        terminal_input_thread = threading.Thread(target=self._terminal_input, daemon=True)
        terminal_input_thread.start()

        print("[STATUS] Establishing connections")

        # Wait for threads to finish
        to_client_request_thread.join()
        from_client_request_thread.join()
        from_clients_thread.join()
        terminal_input_thread.join()

        print("[STATUS] Closed connection gate")

    def close_connections_listener(self) -> None:
        self._to_client_request.close()
        self._from_client_request.close()

    def _dispatch_to_client_request(self) -> None:
        """Dispatch client's connection for receiving game state updates from server
        """
        # Listen for client connection
        self._to_client_request.listen()

        while self._establishing_connections:
            # Check for connection request
            readable, _, _ = select([self._to_client_request], [], [self._to_client_request], 0.1)

            for connection in readable:
                client_conn, client_addr = connection.accept()
                client_conn.setblocking(False)

                # get connection name from client
                [serialized_payload] = receive([client_conn])
                payload = ClientPayload.deserialize(serialized_payload)

                if payload.client_name in self.to_client_connections:
                    data = {}
                    data["type"] = "status"
                    data["status"] = "failed"
                    send([client_conn], data)
                    print(f"[WARNING] Connection name exists for {payload.client_name}")
                else:
                    if payload.client_id not in self._client_name_to_id:
                        self._client_name_to_id[payload.client_name] = payload.client_id
                    self.to_client_connections[payload.client_name] = client_conn
                    data = {}
                    data["type"] = "status"
                    data["status"] = "succeeded"
                    send([client_conn], data)
                    print("Sending replies to [" + client_addr[0] + ", " + str(client_addr[1]) + ']')

    def _dispatch_from_client_request(self) -> None:
        """Establish connection to receive clients' command
        """
        # Listen for client connection
        self._from_client_request.listen()

        while self._establishing_connections:
            # Check for connection request
            readable, _, _ = select([self._from_client_request], [], [self._from_client_request], 0.1)

            for connection in readable:
                client_conn, client_addr = connection.accept()
                client_conn.setblocking(False)

                # get connection name from client
                [serialized_payload] = receive([client_conn])
                payload = ClientPayload.deserialize(serialized_payload)

                if payload.client_name in self.to_client_connections.values():
                    data = {}
                    data["type"] = "status"
                    data["status"] = "failed"
                    send([client_conn], data)
                    print(f"[WARNING] Connection name existed for {payload.client_name}")
                else:
                    if payload.client_id not in self._client_name_to_id:
                        self._client_name_to_id[payload.client_name] = payload.client_id
                    self.from_client_connections[client_conn] = payload.client_name
                    data = {}
                    data["type"] = "status"
                    data["status"] = "succeeded"
                    send([client_conn], data)
                    print("Receiving commands from [" + payload.client_name + ", " + client_addr[0] + ", " + str(
                        client_addr[1]) + ']')

    def _from_clients(self) -> None:
        """Get request and status from clients
        """
        while self._establishing_connections:
            all_data = receive(self.from_client_connections, 0.1)

            for sender_name, data in all_data.items():
                if data["type"] == "request":
                    if data["request"] == "close":
                        self.to_client_connections[sender_name].close()
                        del self.to_client_connections[sender_name]

                        for connection, name in self.from_client_connections.items():
                            if name == sender_name:
                                connection.close()
                                del self.from_client_connections[connection]
                                break

                        num_connections = len(self.to_client_connections)
                        print(f"Closed connection to {sender_name}, {num_connections} connections remain")
                elif data["type"] == "status" and data["status"] == "ready":
                    print(f"[INFO] Client {sender_name} is ready.")
                    self._num_required_connections -= 1
                    if self._num_required_connections <= 0:
                        # Get ready to start the next task
                        self._establishing_connections = False

    def _terminal_input(self) -> None:
        while True:

            if not self._establishing_connections:
                # When all required clients have connected to the server.
                print("")
                print("All clients have connected to the server. Press Enter to move on to the next task.")
                input()
                break
