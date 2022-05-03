import pygame

from .utils import instruction


def ping_pong_task_competitive_instruction(screen):
    image = pygame.image.load("instructions/images/TomCat_BaselineInstructions_PingPongGame_Pt1.png")
    instruction(image, screen)
