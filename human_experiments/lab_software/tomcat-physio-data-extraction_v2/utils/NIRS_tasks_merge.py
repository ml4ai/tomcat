import numpy as np
from termcolor import colored

def create_task_label_distribution(block, start, end, state):
    #Gets the index of the start and end of the rest state with respect to unix_time of the block. 
    #This is used to create a distribution of the task labels for the task.
    start_index = np.searchsorted(block['unix_time'].values, start)
    end_index = np.searchsorted(block['unix_time'].values, end)

    range_ = list(range(start_index, end_index))
    state = [state] * len(range_)
    state = {i: x for i, x in enumerate(state, start_index)}

    # block['event_type'] = block.index.map(state)

    print("Index of state_start:", start_index)
    print("Index of state_end:", end_index)
    return start_index, end_index, state

def NIRS_tasks_merge(lion_0297_block, tiger_0239_block, leopard_0171_block, task):
    iMacs = {'lion_0297_block': lion_0297_block, 'tiger_0239_block': tiger_0239_block, 'leopard_0171_block': leopard_0171_block}

    start_stop_time = {}  # Create a dictionary that would contain the start and stop time of each task followed by the task name and the participant id.
    idx = 0  # Index for the dictionary

    for iMac_name, iMac in iMacs.items():
        start_index, end_index, state_series = create_task_label_distribution(iMac, rest_state[0], rest_state[1], state='rest_state') #1. Create task label distribution for rest state
        print(colored("[Status] Labeling dataframe with rest state for", "green", attrs=["bold"]), colored(iMac_name, "blue", attrs=["bold"]))
        start_stop_time[idx] = {
                        "state": "rest_state",
                        "participant": None,
                        "start_time":start_index,
                        "end_time": end_index,
                    }
        idx += 1

        start_index, end_index, state_series = create_task_label_distribution(iMac, rest_state[2], rest_state[3], state='finger_tapping') #2. Create task label distribution for finger tapping



        
