import numpy as np
import pandas as pd
from termcolor import colored
from utils.create_time_distribution import create_time_distribution

def NIRS_tasks_merge(lion_0297_block_1_NIRS, tiger_0239_block_1_NIRS, leopard_0171_block_1_NIRS, lion_0297_block_2_NIRS, tiger_0239_block_2_NIRS, leopard_0171_block_2_NIRS):
    lion_0297_block_NIRS = pd.concat([lion_0297_block_1_NIRS, lion_0297_block_2_NIRS], ignore_index=True)
    tiger_0239_block_NIRS = pd.concat([tiger_0239_block_1_NIRS, tiger_0239_block_2_NIRS], ignore_index=True)
    leopard_0171_block_NIRS = pd.concat([leopard_0171_block_1_NIRS, leopard_0171_block_2_NIRS], ignore_index=True)

    lion_0297_block_NIRS = create_time_distribution(lion_0297_block_NIRS)
    tiger_0239_block_NIRS = create_time_distribution(tiger_0239_block_NIRS)
    leopard_0171_block_NIRS = create_time_distribution(leopard_0171_block_NIRS)
    return lion_0297_block_NIRS, tiger_0239_block_NIRS, leopard_0171_block_NIRS

def EEG_tasks_merge(lion_0297_block_1_EEG, tiger_0239_block_1_EEG, leopard_0171_block_1_EEG, lion_0297_block_2_EEG, tiger_0239_block_2_EEG, leopard_0171_block_2_EEG):
    lion_0297_block_EEG = pd.concat([lion_0297_block_1_EEG, lion_0297_block_2_EEG], ignore_index=True)
    tiger_0239_block_EEG = pd.concat([tiger_0239_block_1_EEG, tiger_0239_block_2_EEG], ignore_index=True)
    leopard_0171_block_EEG = pd.concat([leopard_0171_block_1_EEG, leopard_0171_block_2_EEG], ignore_index=True)

    lion_0297_block_EEG = create_time_distribution(lion_0297_block_EEG)
    tiger_0239_block_EEG = create_time_distribution(tiger_0239_block_EEG)
    leopard_0171_block_EEG = create_time_distribution(leopard_0171_block_EEG)
    return lion_0297_block_EEG, tiger_0239_block_EEG, leopard_0171_block_EEG