import numpy as np
import pandas as pd
from termcolor import colored
from utils.create_time_distribution import create_time_distribution

def NIRS_tasks_merge(lion_0297_block_1, tiger_0239_block_1, leopard_0171_block_1, lion_0297_block_2, tiger_0239_block_2, leopard_0171_block_2):
    lion_0297_block = pd.concat([lion_0297_block_1, lion_0297_block_2], ignore_index=True)
    tiger_0239_block = pd.concat([tiger_0239_block_1, tiger_0239_block_2], ignore_index=True)
    leopard_0171_block = pd.concat([leopard_0171_block_1, leopard_0171_block_2], ignore_index=True)

    lion_0297_block = create_time_distribution(lion_0297_block)
    tiger_0239_block = create_time_distribution(tiger_0239_block)
    leopard_0171_block = create_time_distribution(leopard_0171_block)
    return lion_0297_block, tiger_0239_block, leopard_0171_block


        
