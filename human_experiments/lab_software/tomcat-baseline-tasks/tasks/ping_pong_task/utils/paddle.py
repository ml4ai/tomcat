import pygame

from .constants import LEFT_TEAM


class Paddle(pygame.sprite.Sprite):
    """
    Paddle pygame sprite controlled by the client.
    """
    def __init__(self,
                 position: tuple,
                 paddle_width: int,
                 paddle_height: int,
                 color: tuple = (0, 0, 0),
                 upper_bound: int = 0,
                 lower_bound: int = 0,
                 paddle_speed_scaling: float = 1.0,
                 paddle_max_speed = None,
                 team: int = LEFT_TEAM):
        super().__init__()

         # Store game information
        self._upper_bound = upper_bound
        self._lower_bound = lower_bound
        self._paddle_speed_scaling = paddle_speed_scaling
        self._paddle_max_speed = paddle_max_speed

        self.team = team

        # Set up pygame sprite
        self.image = pygame.Surface((paddle_width, paddle_height))
        self.image.fill((0, 0 ,0))

        self.mask = pygame.mask.from_surface(self.image)

        pygame.draw.rect(self.image, color, (0, 0, paddle_width, paddle_height))

        self.rect = self.image.get_rect()
        self.rect.x, self.rect.y = position

    def update_location(self, change: int):
        speed = int(change * self._paddle_speed_scaling)

        if self._paddle_max_speed is not None:
            speed = max(-self._paddle_max_speed, min(self._paddle_max_speed, speed))

        self.rect.y = max(self._lower_bound, min(self._upper_bound, self.rect.y + speed))
