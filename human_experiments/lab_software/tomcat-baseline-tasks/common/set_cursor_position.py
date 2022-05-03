import pygame


def set_cursor_position(new_x: int, new_y: int) -> None:
    pygame.mouse.set_pos((new_x, new_y))
    pygame.event.get()
