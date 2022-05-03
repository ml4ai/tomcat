import pygame
from common import COLOR_BACKGROUND
from config import CLIENT_WINDOW_HEIGHT, CLIENT_WINDOW_WIDTH


def render_image_center(image_path: str, screen, y_offset: int = 0, refresh: bool = False):
    image = pygame.image.load(image_path)

    if refresh:
        screen.fill(COLOR_BACKGROUND)

    image_rect = image.get_rect(center=(CLIENT_WINDOW_WIDTH / 2, CLIENT_WINDOW_HEIGHT / 2 + y_offset))
    screen.blit(image, image_rect)

    pygame.display.flip()
