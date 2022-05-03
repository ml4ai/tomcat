import socket

from .receive import receive
from .send import send


class Client:
    """Establish connection to server communication channels
    """
    def __init__(self, host: str, port: int, client_name: str) -> None:
        self.client_name = client_name

        # connect to the channel to receive data from server
        self.from_server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.from_server.connect((host, port))
        self.from_server.setblocking(False)

        # notify the server the name of the client through this channel
        send([self.from_server], self.client_name)
        [data] = receive([self.from_server])
        if data["type"] != "status" or data["status"] == "failed":
            raise RuntimeError("[ERROR] Connection to from_server failed")

        # connect to the channel to send data to server
        self.to_server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.to_server.connect((host, port + 1))
        self.to_server.setblocking(False)

        # notify the server the name of the client through this channel
        send([self.to_server], self.client_name)
        [data] = receive([self.to_server])
        if data["type"] != "status" or data["status"] == "failed":
            raise RuntimeError("[ERROR] Connection to to_server failed")

        print("[INFO] Connected to server")

    def close(self):
        """Close connection to server
        """
        data = {}
        data["type"] = "request"
        data["request"] = "close"

        send([self.to_server], data)

        self.from_server.close()
        self.to_server.close()

        print("[INFO] Closed connection to server")
