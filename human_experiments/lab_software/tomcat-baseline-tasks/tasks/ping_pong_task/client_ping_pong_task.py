import threading

import pygame
from common import (COLOR_BACKGROUND, COLOR_DIM, COLOR_FOREGROUND,
                    COLOR_PLAYER, render_blank_screen)
from config import (BLANK_SCREEN_COUNT_DOWN_MILLISECONDS, CLIENT_WINDOW_HEIGHT,
                    CLIENT_WINDOW_WIDTH, UPDATE_RATE)
from network import receive, send

from .config_ping_pong_task import (COUNT_DOWN_MESSAGE,
                                    SHOW_SCORE_COUNT_DOWN_MILLISECONDS)
from .utils import BALL_SIZE, WINDOW_HEIGHT, WINDOW_WIDTH, Ball, Paddle


class ClientPingPongTask:
    def __init__(self, from_server, to_server, screen, client_name, easy_mode: bool = True) -> None:
        self._from_server = from_server
        self._to_server = to_server
        self._screen = screen
        self._client_name = client_name

        if easy_mode:
            from . import config_easy_mode as cfg
        else:
            from . import config_hard_mode as cfg

        self._paddle_height = cfg.PADDLE_HEIGHT
        self._paddle_width = cfg.PADDLE_WIDTH

        self._game_y_lower_bound = int((CLIENT_WINDOW_HEIGHT - WINDOW_HEIGHT) / 2)
        self._game_y_upper_bound = self._game_y_lower_bound + WINDOW_HEIGHT

        self._game_x_lower_bound = int((CLIENT_WINDOW_WIDTH - WINDOW_WIDTH) / 2)
        self._game_x_upper_bound = self._game_x_lower_bound + WINDOW_WIDTH

        self._running = False

    def run(self):
        self._running = True

        client_input_thread = threading.Thread(target=self._client_input_handle, daemon=True)
        client_input_thread.start()

        pygame.event.set_grab(True)

        print("[STATUS] Running ping pong task")

        while self._running:
            pygame.event.get()

            data = receive([self._from_server], 0.0)
            if not data:
                continue
            else:
                [data] = data

            if data["type"] == "state":
                state = data["state"]
                score_left = data["score_left"]
                score_right = data["score_right"]
                seconds = data["seconds"]
                game_started = data["started"]
            elif data["type"] == "request":
                if data["request"] == "end":
                    self._running = False

                    render_blank_screen(self._screen, BLANK_SCREEN_COUNT_DOWN_MILLISECONDS)

                    # display game score
                    score_left = data["score_left"]
                    score_right = data["score_right"]

                    font = pygame.font.Font(None, 50)

                    if score_left > score_right:
                        message = f"Left team won (left {score_left} - right {score_right})"
                    elif score_right > score_left:
                        message = f"Right team won (left {score_left} - right {score_right})"
                    else:
                        message = f"Tie (left {score_left} - right {score_right})"

                    score_message = font.render(message, 1, COLOR_FOREGROUND)
                    score_message_rect = score_message.get_rect(center=(CLIENT_WINDOW_WIDTH / 2, CLIENT_WINDOW_HEIGHT / 2))
                    self._screen.blit(score_message, score_message_rect)

                    pygame.display.flip()

                    pygame.time.wait(SHOW_SCORE_COUNT_DOWN_MILLISECONDS)
                    break

            self._screen.fill(COLOR_BACKGROUND)

            # Add sprites to sprite group
            all_sprites_list = pygame.sprite.Group()
            for name, position in state.items():
                if name == "ball":
                    object = Ball(BALL_SIZE)
                    object.rect.x, object.rect.y = position
                elif name == self._client_name:
                    object = Paddle(position, 
                                    paddle_width=self._paddle_width, 
                                    paddle_height=self._paddle_height, 
                                    color=COLOR_PLAYER)
                else:
                    object = Paddle(position, 
                                    paddle_width=self._paddle_width, 
                                    paddle_height=self._paddle_height, 
                                    color=COLOR_FOREGROUND)

                all_sprites_list.add(object)

            # Draw sprite group
            all_sprites_list.draw(self._screen)

            # Draw game border
            pygame.draw.line(self._screen,
                             COLOR_DIM,
                             (self._game_x_lower_bound, self._game_y_lower_bound),
                             (self._game_x_upper_bound, self._game_y_lower_bound))
            pygame.draw.line(self._screen,
                             COLOR_DIM,
                             (self._game_x_lower_bound, self._game_y_upper_bound),
                             (self._game_x_upper_bound, self._game_y_upper_bound))
            pygame.draw.line(self._screen,
                             COLOR_DIM,
                             (self._game_x_lower_bound, self._game_y_lower_bound),
                             (self._game_x_lower_bound, self._game_y_upper_bound))
            pygame.draw.line(self._screen,
                             COLOR_DIM,
                             (self._game_x_upper_bound, self._game_y_lower_bound),
                             (self._game_x_upper_bound, self._game_y_upper_bound))

            # Display scores:
            font = pygame.font.Font(None, 74)
            text_score_left = font.render(str(score_left), 1, COLOR_FOREGROUND)
            text_score_left_rect = text_score_left.get_rect(center=(self._game_x_lower_bound, self._game_y_lower_bound - 20))
            self._screen.blit(text_score_left, text_score_left_rect)

            text_score_right = font.render(str(score_right), 1, COLOR_FOREGROUND)
            text_score_right_rect = text_score_right.get_rect(center=(self._game_x_upper_bound, self._game_y_lower_bound - 20))
            self._screen.blit(text_score_right, text_score_right_rect)

            # Display timer
            timer = font.render(str(seconds), 1, COLOR_DIM)
            timer_rect = timer.get_rect(center=(CLIENT_WINDOW_WIDTH / 2, self._game_y_lower_bound - 20))
            self._screen.blit(timer, timer_rect)

            # Display starting message
            if not game_started:
                font = pygame.font.Font(None, 50)
                timer = font.render(COUNT_DOWN_MESSAGE, 1, COLOR_DIM)
                timer_rect = timer.get_rect(center=(CLIENT_WINDOW_WIDTH / 2, self._game_y_upper_bound + 30))
                self._screen.blit(timer, timer_rect)

            pygame.display.flip()

        # Wait for threads to finish
        client_input_thread.join()

        pygame.event.set_grab(False)

        print("[STATUS] Ping pong task ended")

    def _client_input_handle(self):
        """
        Send user's input command to server
        """
        clock = pygame.time.Clock()
        while self._running:
            _, mouse_y_change = pygame.mouse.get_rel()

            data = {}
            data["type"] = "change"
            data["sender"] = self._client_name
            data["change"] = mouse_y_change

            send([self._to_server], data)

            clock.tick(UPDATE_RATE)
