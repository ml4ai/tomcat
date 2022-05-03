import pygame


def cursor_visibility(visible: bool) -> None:
    pygame.mouse.set_visible(visible)
    pygame.event.get()
