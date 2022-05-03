import pygame

from .utils import instruction


def ping_pong_task_cooperative_instruction(screen):
    image = pygame.image.load("instructions/images/TomCat_BaselineInstructions_PingPongGame_Pt2.png")
    instruction(image, screen)
