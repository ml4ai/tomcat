import threading

import pygame
from common import (COLOR_BACKGROUND, COLOR_DIM, COLOR_FOREGROUND,
                    COLOR_PLAYER, COLOR_PLAYER_DIM)
from config import UPDATE_RATE
from network import receive, send

from .config_finger_tapping_task import COUNT_DOWN_MESSAGE, SQUARE_WIDTH
from .utils import PlayerSquare


class ClientFingerTappingTask:
    def __init__(self, from_server, to_server, screen, client_name) -> None:
        self._from_server = from_server
        self._to_server = to_server
        self._screen = screen
        self._client_name = client_name

        self._state = None

        self._running = False

    def run(self):
        self._running = True

        client_input_thread = threading.Thread(
            target=self._client_input_handle, daemon=True)
        client_input_thread.start()

        # compute coordinate to place the player squares
        win_width, win_height = pygame.display.get_surface().get_size()
        main_player_coordinate = (
            (win_width - SQUARE_WIDTH) / 2, (win_height / 2) - SQUARE_WIDTH - 1)
        other_player_height = (win_height / 2) + 1
        other_player_width_offset = (SQUARE_WIDTH / 2) + 1

        print("[STATUS] Running finger tapping task")

        while self._running:
            pygame.event.get()

            # get state from server
            data = receive([self._from_server], 0.0)
            if not data:
                continue
            else:
                [data] = data

            if data["type"] == "state":
                self._state = data["state"]
                reveal_others = data["reveal"]
                session_index = data["session_index"]
            elif data["type"] == "request":
                if data["request"] == "end":
                    self._running = False
                    break

            num_other_players = len(self._state) - 1
            player_counter = 0

            self._screen.fill(COLOR_BACKGROUND)

            # Add sprites to sprite group
            all_sprites_list = pygame.sprite.Group()
            for name, state in self._state.items():
                if name == self._client_name:
                    color = COLOR_PLAYER if state else COLOR_PLAYER_DIM
                    subject = PlayerSquare(main_player_coordinate, color)
                    all_sprites_list.add(subject)
                elif reveal_others:
                    if num_other_players == 1:
                        color = COLOR_FOREGROUND if state else COLOR_DIM
                        subject = PlayerSquare(
                            (main_player_coordinate[0], other_player_height), color)
                        all_sprites_list.add(subject)
                    elif player_counter == 0:
                        color = COLOR_FOREGROUND if state else COLOR_DIM
                        subject = PlayerSquare(
                            (main_player_coordinate[0] - other_player_width_offset, other_player_height), color)
                        all_sprites_list.add(subject)
                        player_counter += 1
                    else:
                        color = COLOR_FOREGROUND if state else COLOR_DIM
                        subject = PlayerSquare(
                            (main_player_coordinate[0] + other_player_width_offset, other_player_height), color)
                        all_sprites_list.add(subject)

            # Draw sprite group
            all_sprites_list.draw(self._screen)

            if session_index < 0:
                # Display timer only for practice session
                font = pygame.font.Font(None, 74)
                text = font.render(str(data["seconds"]), 1, COLOR_FOREGROUND)
                text_rect = text.get_rect(
                    center=((win_width / 2), main_player_coordinate[1] - 25))
                self._screen.blit(text, text_rect)

                font = pygame.font.Font(None, 50)
                text = font.render(COUNT_DOWN_MESSAGE, 1, COLOR_FOREGROUND)
                text_rect = text.get_rect(
                    center=((win_width / 2), other_player_height + 250))
                self._screen.blit(text, text_rect)

            pygame.display.flip()

        # Wait for threads to finish
        client_input_thread.join()

        print("[STATUS] Finger tapping task ended")

    def _client_input_handle(self):
        """Send user's input command to server
        """
        clock = pygame.time.Clock()
        while self._running:
            # Get keys pressed by user
            keys = pygame.key.get_pressed()

            if self._state is None:
                continue

            # only send user commands when state does not reflect user's interaction
            data = None

            if keys[pygame.K_SPACE]:
                if self._state[self._client_name] == 0:
                    data = {}
                    data["type"] = "command"
                    data["sender"] = self._client_name
                    data["command"] = "tap"
            elif self._state[self._client_name] == 1:
                data = {}
                data["type"] = "command"
                data["sender"] = self._client_name
                data["command"] = "untap"

            if data is not None:
                send([self._to_server], data, wait_time=0.0)

            clock.tick(UPDATE_RATE)
