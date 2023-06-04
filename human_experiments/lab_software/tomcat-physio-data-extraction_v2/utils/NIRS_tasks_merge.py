import numpy as np
import pandas as pd
from termcolor import colored

def NIRS_tasks_merge(lion_0297_block_1, tiger_0239_block_1, leopard_0171_block_1, lion_0297_block_2, tiger_0239_block_2, leopard_0171_block_2):
    lion_0297_block = pd.concat([lion_0297_block_1, lion_0297_block_2], ignore_index=True)
    tiger_0239_block = pd.concat([tiger_0239_block_1, tiger_0239_block_2], ignore_index=True)
    leopard_0171_block = pd.concat([leopard_0171_block_1, leopard_0171_block_2], ignore_index=True)

    return lion_0297_block, tiger_0239_block, leopard_0171_block


        
