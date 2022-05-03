import pygame

from .utils import instruction


def affective_task_instruction_team(screen):
    image = pygame.image.load("instructions/images/TomCat_BaselineInstructions_AffectiveTask_Team_Pt1.png")
    instruction(image, screen) 

    image = pygame.image.load("instructions/images/TomCat_BaselineInstructions_AffectiveTask_Team_Pt2.png")
    instruction(image, screen)
