import pygame
from ..config import SQUARE_WIDTH


class PlayerSquare(pygame.sprite.Sprite):
    """
    Player square sprite
    """
    def __init__(self, position, color):
        # Set up pygame sprite
        super().__init__()
        self.image = pygame.Surface((SQUARE_WIDTH, SQUARE_WIDTH))
        self.image.fill((0, 0 ,0))
        self.image.set_colorkey((0, 0 ,0))
        pygame.draw.rect(self.image, color, (0, 0, SQUARE_WIDTH, SQUARE_WIDTH))

        self.rect = self.image.get_rect()
        self.rect.x, self.rect.y = position
