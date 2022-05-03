import pygame
from config import UPDATE_RATE
from common import render_text_center

def display_msg_affective_disscussion(screen, msg: str, milliseconds: int):
    start_ticks = pygame.time.get_ticks()

    clock = pygame.time.Clock()
    while pygame.time.get_ticks() - start_ticks < milliseconds:
        render_text_center(msg, (1250, 90), screen, font_size = 55 , x_offset = 0, y_offset=0)
        pygame.event.get()
        clock.tick(UPDATE_RATE)
        
