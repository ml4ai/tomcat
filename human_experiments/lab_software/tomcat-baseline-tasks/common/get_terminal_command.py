import sys
from select import select
from typing import Optional


def get_terminal_command(wait_time: Optional[float] = None) -> Optional[str]:
    terminal, _, _ = select([sys.stdin], [], [], wait_time)
    return None if not terminal else terminal[0].readline().strip()
