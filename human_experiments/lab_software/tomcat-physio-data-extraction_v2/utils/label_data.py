

def label_data(lion_0297_block, tiger_0239_block, leopard_0171_block, markers):
    for key, value in markers.items():
        if value['state'] == 'rest_state':
            start_index = lion_0297_block["unix_time"].searchsorted(value['start_time'])
            end_index = lion_0297_block["unix_time"].searchsorted(value['end_time'])
            lion_0297_block.loc[start_index:end_index, 'state'] = 'rest_state'
            
            start_index = tiger_0239_block["unix_time"].searchsorted(value['start_time'])
            end_index = tiger_0239_block["unix_time"].searchsorted(value['end_time'])
            tiger_0239_block.loc[start_index:end_index, 'state'] = 'rest_state'
            
            start_index = leopard_0171_block["unix_time"].searchsorted(value['start_time'])
            end_index = leopard_0171_block["unix_time"].searchsorted(value['end_time'])
            leopard_0171_block.loc[start_index:end_index, 'state'] = 'rest_state'

        if value['state'] == 'finger_tapping':
            start_index = lion_0297_block["unix_time"].searchsorted(value['start_time'])
            end_index = lion_0297_block["unix_time"].searchsorted(value['end_time'])
            lion_0297_block.loc[start_index:end_index, 'state'] = 'finger_tapping'

            start_index = tiger_0239_block["unix_time"].searchsorted(value['start_time'])
            end_index = tiger_0239_block["unix_time"].searchsorted(value['end_time'])
            tiger_0239_block.loc[start_index:end_index, 'state'] = 'finger_tapping'

            start_index = leopard_0171_block["unix_time"].searchsorted(value['start_time'])
            end_index = leopard_0171_block["unix_time"].searchsorted(value['end_time'])
            leopard_0171_block.loc[start_index:end_index, 'state'] = 'finger_tapping'
        
        if value['state'] == 'affective_task_individual' and value['participant'] == 'lion':
            start_index = lion_0297_block["unix_time"].searchsorted(value['start_time'])
            end_index = lion_0297_block["unix_time"].searchsorted(value['end_time'])
            lion_0297_block.loc[start_index:end_index, 'state'] = 'affective_task_individual'
        
        if value['state'] == 'affective_task_individual' and value['participant'] == 'tiger':
            start_index = tiger_0239_block["unix_time"].searchsorted(value['start_time'])
            end_index = tiger_0239_block["unix_time"].searchsorted(value['end_time'])
            tiger_0239_block.loc[start_index:end_index, 'state'] = 'affective_task_individual'
        
        if value['state'] == 'affective_task_individual' and value['participant'] == 'leopard':
            start_index = leopard_0171_block["unix_time"].searchsorted(value['start_time'])
            end_index = leopard_0171_block["unix_time"].searchsorted(value['end_time'])
            leopard_0171_block.loc[start_index:end_index, 'state'] = 'affective_task_individual'

        if value['state'] == 'affective_task_team':
            start_index = lion_0297_block["unix_time"].searchsorted(value['start_time'])
            end_index = lion_0297_block["unix_time"].searchsorted(value['end_time'])
            lion_0297_block.loc[start_index:end_index, 'state'] = 'affective_task_team'
            
            start_index = tiger_0239_block["unix_time"].searchsorted(value['start_time'])
            end_index = tiger_0239_block["unix_time"].searchsorted(value['end_time'])
            tiger_0239_block.loc[start_index:end_index, 'state'] = 'affective_task_team'
            
            start_index = leopard_0171_block["unix_time"].searchsorted(value['start_time'])
            end_index = leopard_0171_block["unix_time"].searchsorted(value['end_time'])
            leopard_0171_block.loc[start_index:end_index, 'state'] = 'affective_task_team'

        if value['state'] == 'ping_pong_competetive_0' and value['participant'] == {'tiger', 'lion'}:
            start_index = lion_0297_block["unix_time"].searchsorted(value['start_time'])
            end_index = lion_0297_block["unix_time"].searchsorted(value['end_time'])
            lion_0297_block.loc[start_index:end_index, 'state'] = 'ping_pong_competetive_0'

            start_index = tiger_0239_block["unix_time"].searchsorted(value['start_time'])
            end_index = tiger_0239_block["unix_time"].searchsorted(value['end_time'])
            tiger_0239_block.loc[start_index:end_index, 'state'] = 'ping_pong_competetive_0'
        
        if value['state'] == 'ping_pong_competetive_1' and value['participant'] == {'leopard'}:
            start_index = leopard_0171_block["unix_time"].searchsorted(value['start_time'])
            end_index = leopard_0171_block["unix_time"].searchsorted(value['end_time'])
            leopard_0171_block.loc[start_index:end_index, 'state'] = 'ping_pong_competetive_1'

        if value['state'] == 'ping_pong_cooperative_0': 
            start_index = lion_0297_block["unix_time"].searchsorted(value['start_time'])
            end_index = lion_0297_block["unix_time"].searchsorted(value['end_time'])
            print(start_index, end_index)
            lion_0297_block.loc[start_index:end_index, 'state'] = 'ping_pong_cooperative_0'

            start_index = tiger_0239_block["unix_time"].searchsorted(value['start_time'])
            end_index = tiger_0239_block["unix_time"].searchsorted(value['end_time'])
            print(start_index, end_index)
            tiger_0239_block.loc[start_index:end_index, 'state'] = 'ping_pong_cooperative_0'

            start_index = leopard_0171_block["unix_time"].searchsorted(value['start_time'])
            end_index = leopard_0171_block["unix_time"].searchsorted(value['end_time'])
            print(start_index, end_index)
            leopard_0171_block.loc[start_index:end_index, 'state'] = 'ping_pong_cooperative_0'

        if value['state'] == 'hands_on_training':
            start_index = lion_0297_block["unix_time"].searchsorted(value['start_time'])
            end_index = lion_0297_block["unix_time"].searchsorted(value['end_time'])
            lion_0297_block.loc[start_index:end_index, 'state'] = 'hands_on_training'

            start_index = tiger_0239_block["unix_time"].searchsorted(value['start_time'])
            end_index = tiger_0239_block["unix_time"].searchsorted(value['end_time'])
            tiger_0239_block.loc[start_index:end_index, 'state'] = 'hands_on_training'

            start_index = leopard_0171_block["unix_time"].searchsorted(value['start_time'])
            end_index = leopard_0171_block["unix_time"].searchsorted(value['end_time'])
            leopard_0171_block.loc[start_index:end_index, 'state'] = 'hands_on_training'

        if value['state'] == 'saturn_a':
            start_index = lion_0297_block["unix_time"].searchsorted(value['start_time'])
            end_index = lion_0297_block["unix_time"].searchsorted(value['end_time'])
            lion_0297_block.loc[start_index:end_index, 'state'] = 'saturn_a'

            start_index = tiger_0239_block["unix_time"].searchsorted(value['start_time'])
            end_index = tiger_0239_block["unix_time"].searchsorted(value['end_time'])
            tiger_0239_block.loc[start_index:end_index, 'state'] = 'saturn_a'

            start_index = leopard_0171_block["unix_time"].searchsorted(value['start_time'])
            end_index = leopard_0171_block["unix_time"].searchsorted(value['end_time'])
            leopard_0171_block.loc[start_index:end_index, 'state'] = 'saturn_a'
        
        if value['state'] == 'saturn_b':
            start_index = lion_0297_block["unix_time"].searchsorted(value['start_time'])
            end_index = lion_0297_block["unix_time"].searchsorted(value['end_time'])
            lion_0297_block.loc[start_index:end_index, 'state'] = 'saturn_b'

            start_index = tiger_0239_block["unix_time"].searchsorted(value['start_time'])
            end_index = tiger_0239_block["unix_time"].searchsorted(value['end_time'])
            tiger_0239_block.loc[start_index:end_index, 'state'] = 'saturn_b'

            start_index = leopard_0171_block["unix_time"].searchsorted(value['start_time'])
            end_index = leopard_0171_block["unix_time"].searchsorted(value['end_time'])
            leopard_0171_block.loc[start_index:end_index, 'state'] = 'saturn_b'

    lion_0297_block.to_csv('/Users/calebjonesshibu/Desktop/test/lion_0297_block.csv')        