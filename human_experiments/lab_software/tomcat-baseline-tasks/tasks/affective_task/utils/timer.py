from typing import Callable, List

import pygame

from common import render_text_center

from ..config import DISCUSSION_TIMER

REFRESH_RATE = 20


def timer(seconds: int, callbacks: List[Callable], pre_text: str, screen, display_timer = 1):
    start_ticks = pygame.time.get_ticks()

    clock = pygame.time.Clock()
    while True:
        events = pygame.event.get()

        for callback in callbacks:
            if callback(events):
                return

        seconds_has_passed = (pygame.time.get_ticks() - start_ticks) / 1000.0
        seconds_left_to_count = seconds - seconds_has_passed
        if seconds_left_to_count < 0.0:
            break
        else:
            seconds_left_to_count = 0 if seconds_left_to_count < 0.0 else int(seconds_left_to_count)
            if display_timer == 0: #0 means don't display timer but the timer runs in the background
                continue
            elif display_timer == 1: #1 means display timer
                render_text_center(pre_text + str(seconds_left_to_count + 1), (300, 50), screen, y_offset=-420)
            elif display_timer == 2: #2 means display timer at half time
                if seconds_left_to_count < seconds/2:
                    render_text_center(pre_text + str(seconds_left_to_count + 1), (900, 70), screen, y_offset=-420)

    clock.tick(REFRESH_RATE)
