import pygame
from common import COLOR_BACKGROUND, COLOR_FOREGROUND, notify_ready
from config import CLIENT_WINDOW_HEIGHT, CLIENT_WINDOW_WIDTH

from .utils import FONT_SIZE, READY_MSG


def exit_instruction(to_server, screen):
    """Show exit instructions to client screen

    :param to_server: channel to send data to server
    :param screen: channel for receiving data from server
    """
    screen.fill(COLOR_BACKGROUND)

    font = pygame.font.Font(None, FONT_SIZE)

    text = font.render("Thank you for participating in the baseline tasks", 1, COLOR_FOREGROUND)
    text_rect = text.get_rect(center=(CLIENT_WINDOW_WIDTH / 2, CLIENT_WINDOW_HEIGHT / 2))
    screen.blit(text, text_rect)

    text = font.render(READY_MSG, 1, COLOR_FOREGROUND)
    text_rect = text.get_rect(center=(CLIENT_WINDOW_WIDTH / 2, CLIENT_WINDOW_HEIGHT - 60))
    screen.blit(text, text_rect)

    pygame.display.flip()

    pygame.event.clear()
    while True:
        event = pygame.event.wait()
        if event.type == pygame.KEYDOWN and event.key == pygame.K_SPACE:
            break

    notify_ready(to_server)
