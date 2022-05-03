import pygame
from common import COLOR_FOREGROUND
from config import CLIENT_WINDOW_HEIGHT, CLIENT_WINDOW_WIDTH

FONT_SIZE = 50
BOX_THICKNESS = 3
BOX_TEXT_BUFFER = 5


def submit_button(screen, y_offset_from_center: int, x_offset_from_center: int = 0):
    font = pygame.font.SysFont("Arial", FONT_SIZE)
    text_render = font.render("Submit", 1, COLOR_FOREGROUND)

    _, _, w, h = text_render.get_rect()
    x = int(CLIENT_WINDOW_WIDTH/2) + x_offset_from_center - int(w/2)
    y = int(CLIENT_WINDOW_HEIGHT/2) + y_offset_from_center - int(h/2)

    # top
    pygame.draw.line(screen,
                     COLOR_FOREGROUND,
                     (x - BOX_TEXT_BUFFER, y - BOX_TEXT_BUFFER),
                     (x + w + BOX_TEXT_BUFFER, y - BOX_TEXT_BUFFER),
                     BOX_THICKNESS)
    
    # left
    pygame.draw.line(screen,
                     COLOR_FOREGROUND,
                     (x - BOX_TEXT_BUFFER, y - BOX_TEXT_BUFFER),
                     (x - BOX_TEXT_BUFFER, y + h),
                     BOX_THICKNESS)
    
    # bottom
    pygame.draw.line(screen, 
                     COLOR_FOREGROUND, 
                     (x - BOX_TEXT_BUFFER, y + h), 
                     (x + w + BOX_TEXT_BUFFER, y + h), 
                     BOX_THICKNESS)
    
    # right
    pygame.draw.line(screen, 
                     COLOR_FOREGROUND, 
                     (x + w + BOX_TEXT_BUFFER, y + h), 
                     (x + w + BOX_TEXT_BUFFER, y - BOX_TEXT_BUFFER), 
                     BOX_THICKNESS)

    object = screen.blit(text_render, (x, y))
    pygame.display.flip()

    return object
