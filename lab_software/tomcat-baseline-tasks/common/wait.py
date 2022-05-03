import pygame
from config import UPDATE_RATE


def wait(milliseconds: int) -> None:
    start_ticks = pygame.time.get_ticks()

    clock = pygame.time.Clock()
    while pygame.time.get_ticks() - start_ticks < milliseconds:
        pygame.event.get()
        clock.tick(UPDATE_RATE)
