from typing import Tuple

import pygame
from common import COLOR_BACKGROUND, COLOR_FOREGROUND
from config import CLIENT_WINDOW_HEIGHT, CLIENT_WINDOW_WIDTH

BOX_WIDTH = 157
BOX_HEIGHT = 195
BOX_THICKNESS = 10


class Button:
    def __init__(self, offset: Tuple[int, int], screen):
        x_offset, y_offset = offset
        x = CLIENT_WINDOW_WIDTH / 2 + x_offset - BOX_WIDTH / 2
        y = CLIENT_WINDOW_HEIGHT / 2 + y_offset - BOX_HEIGHT / 2

        self._position = (x, y)
        self._screen = screen

        self.object = pygame.draw.rect(self._screen, pygame.Color(0, 0, 0, 0), pygame.Rect(x, y, BOX_WIDTH, BOX_HEIGHT), BOX_THICKNESS)
        self._selected = False

    def _render(self, no_frame: bool = False):
        x, y = self._position

        if self._selected:
            color = COLOR_FOREGROUND
        elif no_frame:
            color = COLOR_BACKGROUND
        else:
            color = (35, 35, 35)

        pygame.draw.line(self._screen, color, (x, y), (x + BOX_WIDTH , y), BOX_THICKNESS)
        pygame.draw.line(self._screen, color, (x, y - 2), (x, y + BOX_HEIGHT), BOX_THICKNESS)
        pygame.draw.line(self._screen, color, (x, y + BOX_HEIGHT), (x + BOX_WIDTH , y + BOX_HEIGHT), BOX_THICKNESS)
        pygame.draw.line(self._screen, color, (x + BOX_WIDTH , y+BOX_HEIGHT), [x + BOX_WIDTH , y], BOX_THICKNESS)

        pygame.display.flip()

    def select(self):
        self._selected = True
        self._render()

    def unselect(self, no_frame: bool = False):
        self._selected = False
        self._render(no_frame)

    def is_selected(self) -> bool:
        return self._selected
