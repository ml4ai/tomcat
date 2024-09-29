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
    fourcc = cv2.VideoWriter_fourcc(*'H264')
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
    "exp_2022_04_01_13", "exp_2022_04_22_09" 
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

