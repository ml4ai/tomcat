# 0 for self tapping, 1 for collaborative tapping
SESSION = [0, 1, 0, 1]

SECONDS_PER_SESSION = [10.0, 10.0, 10.0, 10.0] # Seconds
DEBUG_SECONDS_PER_SESSION = [2.0, 2.0, 2.0, 2.0] # Seconds

# count down before start
SECONDS_COUNT_DOWN = 10.0 # Seconds
DEBUG_SECONDS_COUNT_DOWN = 2.0 # Seconds

# game sprite
SQUARE_WIDTH = 200 # Pixels

COUNT_DOWN_MESSAGE = "Practice session: Press SPACEBAR and observe the squares"

TOTAL_TIME = sum(SECONDS_PER_SESSION) + SECONDS_COUNT_DOWN
