import numpy as np
from termcolor import colored

def create_task_label_distribution(block, start, end):
    #Gets the index of the start and end of the rest state with respect to unix_time of the block. 
    #This is used to create a distribution of the task labels for the task.
    start_index = np.searchsorted(block['unix_time'].values, start)
    end_index = np.searchsorted(block['unix_time'].values, end)

    print("Index of rest_state_start:", start_index)
    print("Index of rest_state_end:", end_index)

def NIRS_tasks_merge(lion_0297_block, tiger_0239_block, leopard_0171_block, rest_state):
    iMacs = [lion_0297_block, tiger_0239_block, leopard_0171_block]

    for iMac in iMacs:
        iMac = create_task_label_distribution(iMac, rest_state[0], rest_state[1]) #1. Create task label distribution for rest state
        print(colored("[Status] Labeling dataframe with Rest state ", "green", attrs=["bold"]))


        
