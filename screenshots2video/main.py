import cv2
import os

def create_video_fixed_duration(image_folder, video_name, frame_duration=0.1):
    """
    Creates a video from images in a specified folder, displaying each image for a fixed duration.
    Skips invalid or unreadable images.

    Args:
        image_folder (str): Path to the folder containing images.
        video_name (str): Output video file name.
        frame_duration (float): Duration each image is displayed (in seconds). Default is 0.1 seconds.
    """
    # Frame rate based on fixed frame duration
    frame_rate = 1 / frame_duration
    
    # Sort images in folder
    image_files = sorted([f for f in os.listdir(image_folder) if f.endswith('.png')])
    if not image_files:
        print(f"No image files found in the folder: {image_folder}")
        return
    
    # Load the first valid image to get dimensions
    first_image_path = os.path.join(image_folder, image_files[0])
    frame = cv2.imread(first_image_path)
    if frame is None:
        print(f"Could not load the first image: {first_image_path}")
        return

    height, width, layers = frame.shape

    # Define the codec and create a VideoWriter object
    fourcc = cv2.VideoWriter_fourcc(*'mp4v')
    video = cv2.VideoWriter(video_name, fourcc, frame_rate, (width, height))

    # Loop through images and add each one to the video
    for image_file in image_files:
        image_path = os.path.join(image_folder, image_file)
        try:
            frame = cv2.imread(image_path)
            if frame is None:
                raise Exception(f"Invalid or unreadable image: {image_file}")
            
            # Write the frame to the video
            video.write(frame)

        except Exception as e:
            # Print error and skip to the next image
            print(f"Error processing {image_file}: {str(e)}")
            continue

    # Release the video object
    video.release()
    print(f"Video saved as {video_name}")


# List of experiments and stations
exp_list = [
    "exp_2022_04_01_13", "exp_2022_04_22_09", "exp_2022_09_09_12", 
    "exp_2022_09_29_15", "exp_2022_09_30_10", "exp_2022_10_04_09", 
    "exp_2022_10_07_15", "exp_2022_10_14_10", "exp_2022_10_18_10", 
    "exp_2022_10_21_15", "exp_2022_10_24_12", "exp_2022_10_27_10", 
    "exp_2022_10_28_10", "exp_2022_10_31_10", "exp_2022_11_01_10", 
    "exp_2022_11_04_10", "exp_2022_11_07_10", "exp_2022_11_08_11", 
    "exp_2022_11_10_10", "exp_2022_11_14_12", "exp_2022_11_15_13", 
    "exp_2022_11_17_15", "exp_2022_11_18_10", "exp_2022_11_22_10", 
    "exp_2022_12_02_15", "exp_2022_12_05_12", "exp_2023_01_30_13", 
    "exp_2023_01_31_14", "exp_2023_02_03_10", "exp_2023_02_06_13", 
    "exp_2023_02_07_14", "exp_2023_02_10_10", "exp_2023_02_16_14", 
    "exp_2023_02_20_01", "exp_2023_02_20_13", "exp_2023_02_21_14"
]

station_list = ["leopard", "lion", "tiger"]

# Loop through experiments and stations
for exp in exp_list:
    print("start"+str(exp))
    for station in station_list:
        print("\t\tstart"+ str(station))

        # Check for both "Screenshots" and "screenshots" directories
        screenshots_folder = f'/data/tomcat/raw/LangLab/experiments/study_3_pilot/group/{exp}/{station}/Screenshots/'
        screenshots_folder_lower = f'/data/tomcat/raw/LangLab/experiments/study_3_pilot/group/{exp}/{station}/screenshots/'
        
        # Use the directory that exists
        if os.path.exists(screenshots_folder):
            image_folder = screenshots_folder
        elif os.path.exists(screenshots_folder_lower):
            image_folder = screenshots_folder_lower
        else:
            print(f"No screenshots folder found for {exp} - {station}")
            continue
        
        # Set the output video name
        video_name = f'/space/mlyang721/screenshot-videos/{exp}-{station}.mp4'

        # Start generating the video
        create_video_fixed_duration(image_folder, video_name)

