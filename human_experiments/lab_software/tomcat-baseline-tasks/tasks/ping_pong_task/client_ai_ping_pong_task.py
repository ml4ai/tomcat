import threading

from config import UPDATE_RATE
from network import receive, send
from pygame import time

from .utils import BALL_SIZE


class ClientAIPingPongTask:
    def __init__(self, from_server, to_server, client_name, easy_mode: bool = True) -> None:
        self._from_server = from_server
        self._to_server = to_server
        self._client_name = client_name

        if easy_mode:
            from . import config_easy_mode as cfg
        else:
            from . import config_hard_mode as cfg

        self._paddle_height = cfg.PADDLE_HEIGHT
        self._paddle_width = cfg.PADDLE_WIDTH
        self._paddle_max_speed = cfg.AI_PADDLE_MAX_SPEED

        # Monitor paddle's and ball's position
        self._ball_y_position = 0
        self._paddle_y_position = 0

        self._running = False

    def run(self):
        self._running = True

        client_input_thread = threading.Thread(target=self._client_input_handle, daemon=True)
        client_input_thread.start()

        print("[STATUS] Running ping pong task")

        while self._running:
            data = receive([self._from_server], 0.0)
            if not data:
                continue
            else:
                [data] = data

            if data["type"] == "state":
                state = data["state"]
            elif data["type"] == "request":
                if data["request"] == "end":
                    self._running = False
                    break

            for name, position in state.items():
                # The ball's position is at index 0
                if name == "ball":
                    self._ball_y_position = position[1]
                # Get AI's paddle's location
                elif name == self._client_name:
                    self._paddle_y_position = position[1]

        # Wait for threads to finish
        client_input_thread.join()

        print("[STATUS] Ping pong task ended")

    def _client_input_handle(self):
        """
        Send user's input command to server
        """
        clock = time.Clock()
        while self._running:
            ball_y = self._ball_y_position + BALL_SIZE / 2.0
            paddle_y = self._paddle_y_position + self._paddle_height / 2.0

            y_change = max(-self._paddle_max_speed, min(self._paddle_max_speed, int(ball_y) - int(paddle_y)))

            if y_change != 0:
                data = {}
                data["type"] = "change"
                data["sender"] = self._client_name
                data["change"] = y_change
                send([self._to_server], data)

            clock.tick(UPDATE_RATE)
