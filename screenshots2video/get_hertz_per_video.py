import pandas as pd

df = pd.read_csv('tomcat.csv')
grouped = df.groupby(['group_session', 'station'])
results = {}

for (group_session, station), group in grouped:
    group = group.sort_values(by='timestamp_unix')
    first_row = group.iloc[0]
    last_row = group.iloc[-1]
    
    id_diff = last_row['id'] - first_row['id']
    time_diff = last_row['timestamp_unix'] - first_row['timestamp_unix']
    
    if time_diff > 0:
        fps = id_diff / time_diff
        results[(group_session, station)] = abs(fps)
    else:
        fps = None
        results[(group_session, station)] = fps

results_df = pd.DataFrame(
    [(group_session, station, fps) for ((group_session, station), fps) in results.items()],
    columns=['group_session', 'station', 'Frames_Per_Second']
)

results_df.to_csv('fps_results.csv', index=False)

print(results_df)
