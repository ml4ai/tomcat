import pandas as pd
import json
import os.path

# scp -r gauss:/data/tomcat/LangLab/experiments/study_3_pilot/group/exp_2022_11_15_13/baseline_tasks/ .
# $ ssh gauss ls -l /data/tomcat/LangLab/experiments/study_3_pilot/group/exp_2022_11_10_10/lion/face_images/ > listing.txt

experiment_dir = '/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_10_10/'
individual_file = 'individual_00074_1668105048.csv'

meta_file = os.path.join(experiment_dir, 'baseline_tasks/affective/', individual_file)
#'/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_15_13/baseline_tasks/affective/individual_00014_1668547218.csv'

meta_df = pd.read_csv(meta_file, sep=';', usecols=['time', 'monotonic_time', 'human_readable_time', 'image_path',
                                                   'subject_id', 'event_type'])
# print(meta_df)

meta_df.to_csv(os.path.join(experiment_dir, 'useful_timestams/', individual_file))
#'/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_15_13/useful_timestams/affective/individual_00014_1668547218.csv')

# meta_minecraft = '/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_15_13/minecraft/MinecraftData_Trial-2_ID-38075ea6-1243-4c7d-8ef4-92b8cdc28fdb.metadata'

start_end = []

meta_minecraft_dir = os.path.join(experiment_dir, 'minecraft')

lion_faces = os.path.join(experiment_dir, 'lion', 'face_images', 'listing.txt')
lion_faces_df = pd.read_csv(lion_faces, sep='\s+', skiprows=[0], skipfooter=3, engine='python',
                            names=['permission', 'links', 'owner', 'group', 'size', 'month', 'date', 'time', 'file name'])
lion_faces_df['timestamp'] = lion_faces_df['file name'].apply(lambda dt: pd.Timestamp(dt[:-4].replace('_', ':')))
# lion_faces_df[['experiment date', 'experiment time']] = lion_faces_df['file name'].str.split('T', expand=True)
# lion_faces_df[['hour', 'minute', 'second']] = lion_faces_df['experiment time'].str.split('_', expand=True)
# lion_faces_df['second'] = lion_faces_df['second'].apply(lambda sec: float(sec[:-5]))
lion_faces_df.sort_values(by=['timestamp'], inplace=True)
print(lion_faces_df)
lion_faces_df.to_csv(os.path.join(experiment_dir, 'useful_timestams/lion_faces_listing.csv'), index=False)
#'/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_15_13/useful_timestams/lion_faces_listing.csv', index=False)


for filename in os.listdir(meta_minecraft_dir):
    trial = filename.split('_')[1].split('-')[1]
    print(trial)
    meta_minecraft = os.path.join(meta_minecraft_dir, filename)
    # checking if it is a file
    if os.path.isfile(meta_minecraft):
        with open(meta_minecraft) as fp:
            for line in fp:
                # ln = f.readline()
                ln_json = json.loads(line)
                if 'data' in ln_json and 'mission_state' in ln_json['data']:

                    mission_state = ln_json['data']['mission_state']
                    date_time = pd.Timestamp(ln_json['header']['timestamp'])

                    file_name = ''
                    if mission_state == 'Start':
                        # first_frame = min(lion_faces_df[lion_faces_df['date time'] <= date_time.to_datetime64()])
                        file_name = lion_faces_df[lion_faces_df['timestamp'] >= date_time].iloc[0]['file name']
                        print('Start', '\t', date_time, '\t', file_name)
                    elif mission_state == 'Stop':
                        file_name = lion_faces_df[lion_faces_df['timestamp'] <= date_time].iloc[-1]['file name']
                        print('Stop', '\t', date_time, '\t', file_name)
                        # exit()
                    # date_time = ln_json['header']['timestamp'].split('T')
                    # date = date_time[0]
                    # h_m_s = date_time[1][:-1].split(':')
                    # hour = int(h_m_s[0])
                    # minute = int(h_m_s[1])
                    # second = float(h_m_s[2])
                    # print(date, hour, minute, second)
                    row = {'trial': trial,
                           'experiment_id': ln_json['msg']['experiment_id'],
                           'trial_id': ln_json['msg']['trial_id'],
                           'mission': ln_json['data']['mission'],
                           'mission_state': mission_state,
                           'timestamp_1': ln_json['msg']['timestamp'],
                           'timestamp_2': ln_json['header']['timestamp'],
                           '@timestamp': ln_json['@timestamp'],
                           'date_time': date_time,
                           # 'date': date,
                           # 'hour': hour,
                           # 'minute': minute,
                           # 'second': second,
                           'filename': file_name  # ln_json['header']['timestamp'].replace(':', '_') + '.png'
                           }
                    start_end.append(row)

start_end_df = pd.DataFrame(start_end)
start_end_df.sort_values(by=['trial', 'timestamp_1'], inplace=True)
# print(start_end_df)
start_end_df.to_csv(os.path.join(experiment_dir, 'useful_timestams/Minecraft_mission_timestamps.csv'), index=False)
#start_end_df.to_csv('/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_15_13/useful_timestams/Minecraft_mission_timestamps.csv', index=False)


