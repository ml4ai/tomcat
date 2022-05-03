import pygame

from .utils import instruction


def affective_task_instruction_individual(screen):
    image = pygame.image.load("instructions/images/TomCat_BaselineInstructions_AffectiveTask_Individual_Pt1.png")
    instruction(image, screen)

    image = pygame.image.load("instructions/images/TomCat_BaselineInstructions_AffectiveTask_Individual_Pt2.png")
    instruction(image, screen)

    image = pygame.image.load("instructions/images/TomCat_BaselineInstructions_AffectiveTask_Individual_Pt3.png")
    instruction(image, screen)
   
    image = pygame.image.load("instructions/images/TomCat_BaselineInstructions_AffectiveTask_Individual_Pt4.png")
    instruction(image, screen)

    image = pygame.image.load("instructions/images/TomCat_BaselineInstructions_AffectiveTask_Individual_Pt5.png")
    instruction(image, screen) 

    image = pygame.image.load("instructions/images/TomCat_BaselineInstructions_AffectiveTask_Individual_Pt6.png")
    instruction(image, screen) 