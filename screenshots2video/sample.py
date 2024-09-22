import cv2
import os
import pandas as pd

def create_video_fixed_duration(image_files, video_name, frame_duration):
    """
    Creates a video from images in a list, displaying each image for a fixed duration.
    Skips invalid or unreadable images.

    Args:
        image_files (list): List of image file paths.
        video_name (str): Output video file name.
        frame_duration (float): Duration each image is displayed (in seconds).
    """
    frame_rate = 1 / frame_duration
    if not image_files:
        print(f"No image for {video_name}.")
        return
    
    first_image_path = image_files[0]
    frame = cv2.imread(first_image_path)
    if frame is None:
        print(f"Could not load the first image: {first_image_path}")
        return

    height, width, layers = frame.shape

    fourcc = cv2.VideoWriter_fourcc(*'mp4v')
    video = cv2.VideoWriter(video_name, fourcc, frame_rate, (width, height))

    for image_file in image_files:
        try:
            frame = cv2.imread(image_file)
            if frame is None:
                raise Exception(f"Invalid or unreadable image: {image_file}")
            video.write(frame)

        except Exception as e:
            print(f"Error processing {image_file}: {str(e)}")
            continue
    video.release()
    print(f"Video saved as {video_name}")

fps_df = pd.read_csv('fps_results.csv')
fps_dict = {}
for idx, row in fps_df.iterrows():
    group_session = row['group_session']
    station = row['station']
    fps = row['Frames_Per_Second']
    fps_dict[(group_session, station)] = fps

group_sessions = fps_df['group_session'].unique()
stations = fps_df['station'].unique()
for exp in group_sessions:
    for station in stations:
        fps = fps_dict.get((exp, station))
        if fps is None or fps <= 0:
            print(f"No valid FPS found for {exp} - {station}, skipping.")
            continue

        output_dir = f'/space/mlyang721/screenshot-videos/{exp}'
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

        # get images from block_1 and block_2
        block_1_folder = f'/data/tomcat/raw/LangLab/experiments/study_3_pilot/group/{exp}/{station}/screenshots/block_1/'
        block_2_folder = f'/data/tomcat/raw/LangLab/experiments/study_3_pilot/group/{exp}/{station}/screenshots/block_2/'

        # sort (useless, just in case)
        image_files_block_1 = sorted([os.path.join(block_1_folder, f) for f in os.listdir(block_1_folder) if f.endswith('.png')]) if os.path.exists(block_1_folder) else []
        image_files_block_2 = sorted([os.path.join(block_2_folder, f) for f in os.listdir(block_2_folder) if f.endswith('.png')]) if os.path.exists(block_2_folder) else []

        # concatenate
        all_image_files = image_files_block_1 + image_files_block_2
        
        if not all_image_files:
            print(f"No images found for {exp} - {station}.")
            continue

        video_name = f'{output_dir}/{station}.mp4'
        frame_duration = 1 / fps
        create_video_fixed_duration(all_image_files, video_name, frame_duration)
