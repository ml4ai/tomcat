from typing import Optional

import pygame

from .colors import COLOR_BACKGROUND
from .wait import wait


def render_blank_screen(screen, timer_miliseconds: Optional[int]) -> None:
    screen.fill(COLOR_BACKGROUND)
    pygame.display.flip()
    if timer_miliseconds is not None:
        wait(timer_miliseconds)
