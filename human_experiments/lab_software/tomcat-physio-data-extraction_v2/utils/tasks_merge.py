import numpy as np
import pandas as pd
from termcolor import colored
from utils.create_time_distribution import create_time_distribution


def tasks_merge(
    lion_block_1,
    tiger_block_1,
    leopard_block_1,
    lion_block_2,
    tiger_block_2,
    leopard_block_2,
):
    # If block 2 is empty, keep block 1 as is. If not, concatenate blocks 1 and 2
    lion_block = (
        lion_block_1
        if lion_block_2.empty
        else pd.concat([lion_block_1, lion_block_2], ignore_index=True)
    )
    tiger_block = (
        tiger_block_1
        if tiger_block_2.empty
        else pd.concat([tiger_block_1, tiger_block_2], ignore_index=True)
    )
    leopard_block = (
        leopard_block_1
        if leopard_block_2.empty
        else pd.concat([leopard_block_1, leopard_block_2], ignore_index=True)
    )

    # Create time distribution only if the final blocks are not empty
    if not lion_block.empty:
        lion_block = create_time_distribution(lion_block)
    if not tiger_block.empty:
        tiger_block = create_time_distribution(tiger_block)
    if not leopard_block.empty:
        leopard_block = create_time_distribution(leopard_block)

    return lion_block, tiger_block, leopard_block
