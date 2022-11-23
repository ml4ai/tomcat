import pandas as pd
import json
import os.path

# $ scp -r gauss:/data/tomcat/LangLab/experiments/study_3_pilot/group/exp_2022_11_15_13/baseline_tasks/ .
# $ ssh gauss ls -l /data/tomcat/LangLab/experiments/study_3_pilot/group/exp_2022_11_10_10/lion/face_images/ > listing.txt

# https://superuser.com/questions/851416/using-scp-to-transfer-a-txt-file-list-of-files
# $ rsync -av --progress --partial-dir=/tmp --files-from=Minecraft_mission_1_Hands-on\ Training_selected.csv gauss:/data/tomcat/LangLab/experiments/study_3_pilot/group/exp_2022_11_10_10/leopard/face_images/ .

experiment_dir = '/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_10_10/'
individual_file = 'individual_00074_1668105048.csv'

meta_file = os.path.join(experiment_dir, 'baseline_tasks/', 'affective/', individual_file)
#'/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_15_13/baseline_tasks/affective/individual_00014_1668547218.csv'

meta_df = pd.read_csv(meta_file, sep=';', usecols=['time', 'monotonic_time', 'human_readable_time', 'image_path',
                                                   'subject_id', 'event_type'])
# print(meta_df)

meta_df.to_csv(os.path.join(experiment_dir, 'useful_timestamps/', individual_file))
#'/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_15_13/useful_timestams/affective/individual_00014_1668547218.csv')

# meta_minecraft = '/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_15_13/minecraft/MinecraftData_Trial-2_ID-38075ea6-1243-4c7d-8ef4-92b8cdc28fdb.metadata'

# Load image file listings

workstations = ['cheetah', 'leopard', 'lion', 'tiger']
workstation = workstations[1]

image_types = ['face_images', 'screenshots']
image_type = image_types[0]

# 0 - Face images
# 1 - Screenshots
file_listings = [{}, {}]
for idx, image_type in enumerate(image_types):
    print(image_type)
    for workstation in workstations:
        image_listing = os.path.join(experiment_dir, workstation, image_type, 'listing.txt')
        if not os.path.isfile(image_listing):
            continue

        print('\t', workstation)
        file_names_df = pd.read_csv(image_listing, sep='\s+', skiprows=[0], skipfooter=3, engine='python',
                                    names=['permission', 'links', 'owner', 'group', 'size', 'month', 'date', 'time', 'file name'])
        file_names_df['png'] = file_names_df['file name'].apply(lambda file_name: file_name[-5:] == 'Z.png')
        file_names_df = file_names_df[file_names_df['png']]
        file_names_df['timestamp'] = file_names_df['file name'].apply(lambda dt: pd.Timestamp(dt[:-4].replace('_', ':')))
        file_names_df.sort_values(by=['timestamp'], inplace=True)
        file_listings[idx][workstation] = file_names_df
        file_names_df.to_csv(os.path.join(experiment_dir, 'useful_timestamps/', f'{workstation}_{image_type}_listing.csv'), index=False)

# print(file_listings)
# exit()

# Process Minecraft metadata


def map_image_timestamps(_mission_id, _mission_name, _mission_state, _mission_timestamp, _image_file_listings, _image_types, _mission_start_timestamp, _experiment_dir, _n_select_files=100):
    _ws_file_names = {}
    for _idx, _img_listings in enumerate(_image_file_listings):
        _img_type = _image_types[_idx]
        print(_img_type)

        for _ws, _ws_img_listing_df in _img_listings.items():

            _img_file_name = ''
            _n_files = 0
            if _mission_state == 'Start':
                _img_file_name = _ws_img_listing_df[_ws_img_listing_df['timestamp'] >= _mission_timestamp].iloc[0]['file name']
            elif _mission_state == 'Stop':
                _ws_mission_images_df = _ws_img_listing_df[(_ws_img_listing_df['timestamp'] >= _mission_start_timestamp) &
                                                    (_ws_img_listing_df['timestamp'] <= _mission_timestamp)]
                _img_file_name = _ws_mission_images_df.iloc[-1]['file name']
                _n_files = len(_ws_mission_images_df.index)
                _ws_mission_images_df[['size', 'month', 'date', 'time', 'file name', 'timestamp']].to_csv(os.path.join(_experiment_dir, 'useful_timestamps/', f'Minecraft_mission_{_img_type}_{_ws}_{_mission_id}_{_mission_name}.csv'), index=False)
                _ws_mission_images_df.head(_n_select_files)['file name'].to_csv(os.path.join(_experiment_dir, _ws, _img_type, f'Minecraft_mission_{_mission_id}_{_mission_name}_selected.csv'), index=False, header=False)

            print(_ws, _mission_state, '\t', _mission_timestamp, '\t', _img_file_name, '\t', _n_files)
            _ws_file_names[f'{_ws}_{_img_type}'] = _img_file_name

    return _ws_file_names


meta_minecraft_dir = os.path.join(experiment_dir, 'minecraft')

start_end = [{'mission_timestamp': None}]

for filename in os.listdir(meta_minecraft_dir):
    trial = filename.split('_')[1].split('-')[1]
    print(trial)
    meta_minecraft = os.path.join(meta_minecraft_dir, filename)
    # checking if it is a file
    if os.path.isfile(meta_minecraft):
        with open(meta_minecraft) as fp:
            for line in fp:
                ln_json = json.loads(line)
                if 'data' in ln_json and 'mission_state' in ln_json['data']:

                    mission = ln_json['data']['mission']
                    mission_state = ln_json['data']['mission_state']
                    mission_timestamp = pd.Timestamp(ln_json['header']['timestamp'])

                    row = {'trial': trial,
                           'experiment_id': ln_json['msg']['experiment_id'],
                           'trial_id': ln_json['msg']['trial_id'],
                           'mission': mission,
                           'mission_state': mission_state,
                           'timestamp_1': ln_json['msg']['timestamp'],
                           'timestamp_2': ln_json['header']['timestamp'],
                           '@timestamp': ln_json['@timestamp'],
                           'mission_timestamp': mission_timestamp,
                           }

                    row.update(map_image_timestamps(trial, mission, mission_state, mission_timestamp, file_listings, image_types, start_end[-1]['mission_timestamp'], experiment_dir))

                    start_end.append(row)

start_end_df = pd.DataFrame(start_end[1:])
start_end_df.sort_values(by=['trial', 'timestamp_1'], inplace=True)
start_end_df.to_csv(os.path.join(experiment_dir, 'useful_timestamps/', 'Minecraft_mission_timestamps.csv'), index=False)

exit()







lion_faces = os.path.join(experiment_dir, workstation, image_type, 'listing.txt')
lion_faces_df = pd.read_csv(lion_faces, sep='\s+', skiprows=[0], skipfooter=3, engine='python',
                            names=['permission', 'links', 'owner', 'group', 'size', 'month', 'date', 'time', 'file name'])
lion_faces_df['timestamp'] = lion_faces_df['file name'].apply(lambda dt: pd.Timestamp(dt[:-4].replace('_', ':')))
# lion_faces_df[['experiment date', 'experiment time']] = lion_faces_df['file name'].str.split('T', expand=True)
# lion_faces_df[['hour', 'minute', 'second']] = lion_faces_df['experiment time'].str.split('_', expand=True)
# lion_faces_df['second'] = lion_faces_df['second'].apply(lambda sec: float(sec[:-5]))
lion_faces_df.sort_values(by=['timestamp'], inplace=True)
print(lion_faces_df)
lion_faces_df.to_csv(os.path.join(experiment_dir, 'useful_timestamps/', f'{workstation}_faces_listing.csv'), index=False)
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
start_end_df.to_csv(os.path.join(experiment_dir, 'useful_timestamps/', f'{workstation}_Minecraft_mission_timestamps.csv'), index=False)
#start_end_df.to_csv('/Users/manujinda/Documents/manujinda/ubunosx/tomcat_data/exp_2022_11_15_13/useful_timestams/Minecraft_mission_timestamps.csv', index=False)


