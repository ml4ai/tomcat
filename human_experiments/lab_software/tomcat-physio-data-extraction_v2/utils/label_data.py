def update_state(block, state, start_time, end_time):
    start_index = block["unix_time"].searchsorted(start_time)
    end_index = block["unix_time"].searchsorted(end_time)
    block.loc[start_index:end_index, 'state'] = state

def label_data(lion_0297_block, tiger_0239_block, leopard_0171_block, markers):
    block_mapping = {'lion': lion_0297_block, 'tiger': tiger_0239_block, 'leopard': leopard_0171_block}
    
    for value in markers.values():
        state = value['state']
        participants = value.get('participant', {'lion', 'tiger', 'leopard'}) 

        # If 'participants' is None or a string, convert it to a set
        if participants is None or isinstance(participants, str):
            participants = {'lion', 'tiger', 'leopard'} if participants is None else {participants}
            
        for participant in participants:
            if participant in block_mapping:
                update_state(block_mapping[participant], state, value['start_time'], value['end_time'])

    return lion_0297_block, tiger_0239_block, leopard_0171_block