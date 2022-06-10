import csv
import json
import threading
from time import time, monotonic
from datetime import datetime

import pygame
from common import record_metadata, request_clients_end
from config import CLIENT_WINDOW_HEIGHT, CLIENT_WINDOW_WIDTH, UPDATE_RATE
from network import receive, send

from .config_ping_pong_task import (COUNT_DOWN_MESSAGE, SECONDS_COUNT_DOWN,
                                    SESSION_TIME_SECONDS)
from .utils import (BALL_SIZE, LEFT_TEAM, RIGHT_TEAM, WINDOW_HEIGHT,
                    WINDOW_WIDTH, Ball, Paddle)


class ServerPingPongTask:
    def __init__(self,
                 to_client_connections: list,
                 from_client_connection_teams: dict,
                 easy_mode: bool = True,
                 session_name: str = '',
                 data_save_path: str = '') -> None:
        self._to_client_connections = to_client_connections

        if easy_mode:
            from . import config_easy_mode as cfg
        else:
            from . import config_hard_mode as cfg

        self._paddle_height = cfg.PADDLE_HEIGHT
        self._paddle_width = cfg.PADDLE_WIDTH
        self._ball_bounce_on_paddle_scale = cfg.BALL_BOUNCE_ON_PADDLE_SCALE

        self._game_y_lower_bound = int((CLIENT_WINDOW_HEIGHT - WINDOW_HEIGHT) / 2)
        self._game_y_upper_bound = self._game_y_lower_bound + WINDOW_HEIGHT

        self._game_x_lower_bound = int((CLIENT_WINDOW_WIDTH - WINDOW_WIDTH) / 2)
        self._game_x_upper_bound = self._game_x_lower_bound + WINDOW_WIDTH

        metadata = {}
        metadata["left_team"] = []
        metadata["right_team"] = []

        self._from_client_connections = {}
        self._paddles = {}

        from_client_connection_team_left, from_client_connection_team_right = from_client_connection_teams

        # spread the segment distance uniformly
        segment_length_left = int((self._game_y_upper_bound - self._paddle_height - self._game_y_lower_bound) / 
                                  (len(from_client_connection_team_left) + 1))

        segment_length_right = int((self._game_y_upper_bound - self._paddle_height - self._game_y_lower_bound) / 
                                   (len(from_client_connection_team_right) + 1))

        # setup paddles for each team
        for count, (from_client_connection, client_name) in enumerate(from_client_connection_team_left.items()):
            self._from_client_connections[from_client_connection] = client_name
            self._paddles[client_name] = Paddle(position=(self._game_x_lower_bound, 
                                                          self._game_y_lower_bound + segment_length_left * (count + 1)),
                                                          paddle_width=self._paddle_width,
                                                          paddle_height=self._paddle_height,
                                                          upper_bound=self._game_y_upper_bound - self._paddle_height,
                                                          lower_bound=self._game_y_lower_bound,
                                                          paddle_speed_scaling=cfg.PADDLE_SPEED_SCALING,
                                                          paddle_max_speed=cfg.PADDLE_MAX_SPEED,
                                                          team=LEFT_TEAM)
            metadata["left_team"].append(client_name)

        for count, (from_client_connection, client_name) in enumerate(from_client_connection_team_right.items()):
            self._from_client_connections[from_client_connection] = client_name
            self._paddles[client_name] = Paddle(position=(self._game_x_upper_bound - self._paddle_width, 
                                                          self._game_y_lower_bound + segment_length_right * (count + 1)),
                                                          paddle_width=self._paddle_width,
                                                          paddle_height=self._paddle_height,
                                                          upper_bound=self._game_y_upper_bound - self._paddle_height,
                                                          lower_bound=self._game_y_lower_bound,
                                                          paddle_speed_scaling=cfg.PADDLE_SPEED_SCALING,
                                                          paddle_max_speed=cfg.PADDLE_MAX_SPEED,
                                                          team=RIGHT_TEAM)
            metadata["right_team"].append(client_name)

        # setup ball
        self._ball = Ball(BALL_SIZE, cfg.BALL_X_SPEED)
        self._ball.rect.y = self._game_y_lower_bound + int((WINDOW_HEIGHT + BALL_SIZE) / 2)
        self._ball.rect.x = self._game_x_lower_bound + int((WINDOW_WIDTH + BALL_SIZE) / 2)

        self._score_left = 0
        self._score_right = 0

        csv_data_path = data_save_path + "/ping_pong"

        csv_file_name = csv_data_path + '/' + session_name + '_' + str(int(time()))
        
        header = [
            "time",
            "monotonic_time",
            "human_readable_time",
            "score_left",
            "score_right",
            "started",
            "ball_x",
            "ball_y"
        ]

        for client_name in self._paddles.keys():
            header.append(client_name + "_x")
            header.append(client_name + "_y")

        header.append("seconds")

        self._csv_file = open(csv_file_name + ".csv", 'w', newline='')
        self._csv_writer = csv.DictWriter(self._csv_file, delimiter=';', fieldnames = header)
        self._csv_writer.writeheader()

        metadata["client_window_height"] = CLIENT_WINDOW_HEIGHT
        metadata["client_window_width"] = CLIENT_WINDOW_WIDTH
        metadata["session_time_seconds"] = SESSION_TIME_SECONDS
        metadata["seconds_count_down"] = SECONDS_COUNT_DOWN
        metadata["count_down_message"] = COUNT_DOWN_MESSAGE
        metadata["paddle_width"] = self._paddle_width
        metadata["paddle_height"] = self._paddle_height
        metadata["ai_paddle_max_speed"] = cfg.AI_PADDLE_MAX_SPEED
        metadata["paddle_speed_scaling"] = cfg.PADDLE_SPEED_SCALING
        metadata["paddle_max_speed"] = cfg.PADDLE_MAX_SPEED
        metadata["ball_x_speed"] = cfg.BALL_X_SPEED
        metadata["ball_bounce_on_paddle_scale"] = cfg.BALL_BOUNCE_ON_PADDLE_SCALE

        json_file_name = csv_file_name + "_metadata"

        record_metadata(json_file_name, metadata)

        self._running = False

    def run(self):
        self._running = True

        to_client_update_state_thread = threading.Thread(target=self._to_client_update_state, daemon=True)
        to_client_update_state_thread.start()

        from_client_commands_thread = threading.Thread(target=self._from_client_commands, daemon=True)
        from_client_commands_thread.start()

        print("[STATUS] Running ping pong task")

        # Wait for threads to finish
        to_client_update_state_thread.join()
        from_client_commands_thread.join()

        self._csv_file.close()

        extra_data = {}
        extra_data["score_left"] = self._score_left
        extra_data["score_right"] = self._score_right
        request_clients_end(self._to_client_connections, extra_data)

        print("[STATUS] Ping pong task ended")

    def _to_client_update_state(self):
        """Update the state of the game and reply to clients
        """
        counter_target = SECONDS_COUNT_DOWN
        game_started = False

        start_ticks = pygame.time.get_ticks()

        seconds = 0.0

        clock = pygame.time.Clock()
        while self._running:
            # manage timer
            if seconds >= counter_target:
                if game_started:
                    self._running = False
                    break
                else:
                    counter_target = SESSION_TIME_SECONDS
                    start_ticks = pygame.time.get_ticks()
                    game_started = True

            # Update state of the ball
            if game_started:
                self._ball.update()

            # Check for collision between ball and paddles
            paddle_collide_ball = False
            for paddle in self._paddles.values():
                if pygame.sprite.collide_mask(self._ball, paddle):
                    if self._ball.velocity[0] > 0 and paddle.team == LEFT_TEAM:
                        self._ball.velocity[1] = -self._ball.velocity[1]
                        self._ball.rect.x = paddle.rect.x + self._paddle_width
                    elif self._ball.velocity[0] < 0 and paddle.team == RIGHT_TEAM:
                        self._ball.velocity[1] = -self._ball.velocity[1]
                        self._ball.rect.x = paddle.rect.x - BALL_SIZE
                    else:
                        ball_bound_y_velocity = int(((self._ball.rect.y + BALL_SIZE / 2.0) -
                                                    (paddle.rect.y + self._paddle_height / 2.0))
                                                    * self._ball_bounce_on_paddle_scale)
                        
                        # Prevent ball from ever moving pure horizontally
                        ball_bound_y_velocity = 1 if ball_bound_y_velocity == 0 else ball_bound_y_velocity
                        
                        self._ball.bounce(ball_bound_y_velocity)

                        if self._ball.rect.x < CLIENT_WINDOW_WIDTH / 2:
                            self._ball.rect.x = self._game_x_lower_bound + self._paddle_width

                        else:
                            self._ball.rect.x = self._game_x_upper_bound - self._paddle_width - BALL_SIZE

                        paddle_collide_ball = True

                    break

            # If ball has not collided with paddle, check if it collides with the wall
            if not paddle_collide_ball:
                # Collides with right wall
                if self._ball.rect.x >= self._game_x_upper_bound - BALL_SIZE:
                    self._score_left += 1
                    self._ball.bounce()
                    # Offset the ball to avoid collision with paddle
                    self._ball.rect.x = self._game_x_upper_bound - BALL_SIZE

                # Collides left wall
                elif self._ball.rect.x <= self._game_x_lower_bound:
                    self._score_right += 1
                    self._ball.bounce()
                    # Offset the ball to avoid collision with paddle
                    self._ball.rect.x = self._game_x_lower_bound

                # Collides with bottom wall
                elif self._ball.rect.y >= self._game_y_upper_bound - BALL_SIZE:
                    self._ball.rect.y = self._game_y_upper_bound - BALL_SIZE - 1
                    self._ball.velocity[1] = -self._ball.velocity[1]

                # Collides with top wall
                elif self._ball.rect.y <= self._game_y_lower_bound:
                    self._ball.rect.y = self._game_y_lower_bound + 1
                    self._ball.velocity[1] = -self._ball.velocity[1]

            # Track game state
            game_state = {
                "time" : time(),
                "monotonic_time" : monotonic(),
                "human_readable_time" : datetime.utcnow().isoformat() + "Z",
                "score_left" : self._score_left,
                "score_right" : self._score_right,
                "started" : game_started,
                "ball_x" : self._ball.rect.x,
                "ball_y" : self._ball.rect.y
            }

            data = {}
            data["type"] = "state"
            data["score_left"] = self._score_left
            data["score_right"] = self._score_right
            data["started"] = game_started
            data["state"] = {}
            data["state"]["ball"] = (self._ball.rect.x, self._ball.rect.y)

            for client_name, paddle in self._paddles.items():
                data["state"][client_name] = (paddle.rect.x, paddle.rect.y)
                game_state[client_name + "_x"] = paddle.rect.x
                game_state[client_name + "_y"] = paddle.rect.y

            seconds_to_send = int(counter_target) - int(seconds)
            data["seconds"] = 1 if seconds_to_send <= 0 else seconds_to_send
            game_state["seconds"] = data["seconds"]

            # Record game data
            self._csv_writer.writerow(game_state)

            # Send game data to clients
            send(self._to_client_connections, data)

            seconds = (pygame.time.get_ticks() - start_ticks) / 1000.0

            clock.tick(UPDATE_RATE)

    def _from_client_commands(self):
        """Update state of paddles from user commands
        """
        while self._running:
            all_data = receive(self._from_client_connections.keys(), 0.1)

            for data in all_data:
                if data["type"] == "change":
                    self._paddles[data["sender"]].update_location(data["change"])
