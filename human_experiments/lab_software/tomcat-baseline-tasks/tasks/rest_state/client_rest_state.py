from common import (cursor_visibility, render_blank_screen,
                    set_cursor_position, wait)
from config import CLIENT_WINDOW_HEIGHT, CLIENT_WINDOW_WIDTH
from network import receive, send

from .config import (BLANK_SCREEN_MILLISECONDS,
                                    CROSS_SCREEN_MILLISECONDS)
from .utils import timer

class ClientRestState:
    def __init__(self, from_server, to_server, screen):
        self._from_server = from_server
        self._to_server = to_server
        self._screen = screen

    def run(self):
        print("[STATUS] Running Rest state")

        while True:

            [data] = receive([self._from_server])

            if data["type"] == "request":
                if data["request"] == "end":
                    break
            elif data["type"] == "state":
                state = data["state"]
            else:
                # Read the next message
                continue      

            # show a blank screen and a cross before showing an image
            render_blank_screen(self._screen, BLANK_SCREEN_MILLISECONDS)

            wait(CROSS_SCREEN_MILLISECONDS)

            timer(state["rest_timer"], [], "Please sit back, relax and try not to move for: ", self._screen)
            
            response = {"type": "STOP"}

            send([self._to_server], response)

        print("[STATUS] Rest task ended")
