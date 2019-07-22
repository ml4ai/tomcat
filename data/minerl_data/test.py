import minerl
data = minerl.data.make(
    'MineRLNavigate-v0',
    data_dir='.')

# Iterate through a single epoch gathering sequences of at most 32 steps
for current_state, action, reward, next_state, done, metadata \
    in data.sarsd_iter(
        num_epochs=1, max_sequence_len=32, include_metadata=True):

        print(metadata)
        # Print the POV @ the first step of the sequence
        print(current_state['pov'][0])

        # Print the final reward pf the sequence!
        print(reward[-1])

        # Check if final (next_state) is terminal.
        print(done[-1])

        # ... do something with the data.
        print("At the end of trajectories the length"
              "can be < max_sequence_len", len(reward))
