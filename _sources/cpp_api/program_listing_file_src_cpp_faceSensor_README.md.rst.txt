
.. _program_listing_file_src_cpp_faceSensor_README.md:

Program Listing for File README.md
==================================

|exhale_lsh| :ref:`Return to documentation for file <file_src_cpp_faceSensor_README.md>` (``src/cpp/faceSensor/README.md``)

.. |exhale_lsh| unicode:: U+021B0 .. UPWARDS ARROW WITH TIP LEFTWARDS

.. code-block:: markdown

   # Executable for Face Analysis
   
   **Prerequisites**
   
   Install ToMCAT and its dependencies using the following commands:
   
   ```bash
   git clone https://github.com/ml4ai/tomcat
   cd tomcat && ./tools/install
   ```
   
   For more information, visit: https://ml4ai.github.io/tomcat/installation.html
   
   ## Description
   
   The `faceSensor` executable uses the [OpenFace library](https://github.com/TadasBaltrusaitis/OpenFace) for facial 
   action unit recognition, face landmark detection, eye-gaze estimation and head pose estimation. The executable can process
   input webcam live as well as video or image files from the disk. 
   
   ## Instructions
   
   Navigate to the `build/` directory in the tomcat root directory and execute:
   
   ```
   cmake ..
   make -j faceSensor
   ./bin/faceSensor
   ```
   This will start processing the webcam live feed and output the facial features to the standard output in JSON format.
   
   #### Command Line Arguments
   
   One way of interacting with the `faceSensor` executable is through the following command line arguments:
   
   ```
     -h [ --help ]             Show this help message
     --exp_id arg              Set experiment ID
     --trial_id arg            Set trial ID
     --playername arg          Set player name
     --mloc arg                Set OpenFace models directory
     --indent                  Indent output JSON by four spaces (default false)
     --visualize               Enable visualization (default false)
     -f [ --file ] arg (=null) Specify an input video/image file
   ```
   
   **NOTE:** When the `--visualize` flag is set to true, the executable also outputs the visualization of facial landmarks, 
   head pose and eye gaze tracking. To exit visualization and stop the processing of webcam/video, press the letter *q* or *Q*.
   
   #### Example Usage
   
   If you want to extract the facial features from **webcam** feed, set the experiment ID as `563e4567-e89b-12d3-a456-426655440000` 
   and set the trial ID as `123e4567-e89b-12d3-a456-426655440000`, execute the following command on the command line:
   
   ```
   ./bin/faceSensor --exp_id 563e4567-e89b-12d3-a456-426655440000 --trial_id 123e4567-e89b-12d3-a456-426655440000
   ```
   
   If you want to extract the facial features from a **video** file in the location `~/Downloads/video.mp4`, set the player name
   as `Aptiminer1` and enable visualization, execute the following command on the command line:
   
   ```
   ./bin/faceSensor -f ~/Downloads/video.mp4 --playername Aptiminer1 --visualize
   ```
   
   If you want to extract the facial features from an **image** file in the location `~/Downloads/image.jpg`, set the OpenFace 
   models directory as `~/git_repos/tomcat/data/OpenFace_models` and enable indentation of JSON output by four spaces, execute 
   the following command on the command line:
   
   ```
   ./bin/faceSensor -f ~/Downloads/image.jpg --mloc ~/git_repos/tomcat/data/OpenFace_models --indent
   ```
   
   ## Output Format
   
   The `faceSensor` executable uses the `nlohmann-json` library to output the action units, eye landmarks, gaze estimation 
   and pose estimation values. The following is an example JSON message with indentation enabled:
   
   ```
   {
       "data": {
           "action_units": {
               "AU01": {
                   "intensity": 1.5039452395072457,
                   "occurrence": 1.0
               },
               "AU02": {
                   "intensity": 0.7107745056044891,
                   "occurrence": 1.0
               },
               ...
           },
           "frame": 1,
           "gaze": {
               "eye_0": {
                   "x": -0.02601720206439495,
                   "y": 0.2048162817955017,
                   "z": -0.97845458984375
               },
               "eye_1": {
                   "x": -0.1461271494626999,
                   "y": 0.2099267840385437,
                   "z": -0.9667355418205261
               },
               "eye_landmarks": {
                   "2D": {
                       "x": [
                           297.0760498046875,
                           300.1932067871094,
                           ...
                       ],
                       "y": [
                           210.02487182617188,
                           202.84886169433594,
                           ...
                       ]
                   },
                   "3D": {
                       "x": [
                           -13.506591796875,
                           -11.667745590209961,
                           ...
                       ],
                       "y": [
                           -17.661083221435547,
                           -21.884918212890625,
                           ...
                       ],
                       "z": [
                           294.59564208984375,
                           294.53900146484375,
                           ...
                       ]
                   }
               },
               "gaze_angle": {
                   "x": -0.088267482817173,
                   "y": 0.21006907522678375
               }
           },
           "landmark_detection_confidence": "0.97500",
           "landmark_detection_success": true,
           "playername": "Aptiminer1",
           "pose": {
               "location": {
                   "x": 21.459043502807617,
                   "y": 16.071529388427734,
                   "z": 367.04388427734375
               },
               "rotation": {
                   "x": 0.11796540021896362,
                   "y": 0.036553021520376205,
                   "z": 0.0021826198790222406
               }
           }
       },
       "header": {
           "message_type": "observation",
           "timestamp": "2020-08-01T12:25:47.626987Z",
           "version": "0.1"
       },
       "msg": {
           "experiment_id": "563e4567-e89b-12d3-a456-426655440000",
           "source": "faceSensor",
           "sub_type": "state",
           "timestamp": "2020-08-01T12:25:47.626987Z",
           "trial_id": "123e4567-e89b-12d3-a456-426655440000",
           "version": "0.1"
       }
   }
   ```
   
   **NOTE:** This output is in accordance with output of the OpenFace executables (see https://github.com/TadasBaltrusaitis/OpenFace/wiki/Output-Format)
   
   The explanation of each element in the `data` block is given below:
   
   **`action_units`**
   
   The sensor can detect the **intensity** (value ranges from 0 to 5) of 17 action units:
   
   `AU01_r, AU02_r, AU04_r, AU05_r, AU06_r, AU07_r, AU09_r, AU10_r, AU12_r, AU14_r, AU15_r, AU17_r, AU20_r, AU23_r, AU25_r, AU26_r, 
   AU45_r`
   
   And the **occurrence** (0 represents absent, 1 represents present) of 18 action units:
   
   `AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, AU26_c, 
   AU28_c, AU45_c`
   
   `frame` specifies the number of the frame (in case of sequences, ie, webcam and videos)
   
   **`gaze`**
   
   `eye_0` specifies the eye gaze direction vector (`xyz` coordinates) for the leftmost eye in the frame
   
   `eye_1` specifies the eye gaze direction vector (`xyz` coordinates) for the rightmost eye in the frame
   
   `2D` specifies the location of 2D eye region landmarks in pixels (`x_0, ... x_55, y_0, ... y_55` coordinates)
   
   `3D` specifies the location of 3D eye region landmarks in millimeters (`x_0, ... x_55, y_0, ... y_55, z_0, ... z_55` coordinates)
   
   `gaze_angle` specifies the eye gaze direction in radians (`xy` coordinates`) averaged for both the eyes
   
   `landmark_detection_confidence` specifies how confident the tracker is in the current landmark detection estimate
   
   `landmark_detection_success` specifies if tracking was successful 
   
   `playername` specifies the name of player
   
   **`pose`**
   
   `location` specifies the location of the head in millimeters (`xyz` coordinates) with respect to camera
   
   `rotation` specifies the rotation of the head in radians (`xyz` coordinates) with camera being the origin 
   
   The explanation of each element in the `header` block is given below:
   
   `message_type` specifies the type of output message
   
   `timestamp` specifies the time of execution in ISO 8601 format
   
   `version` specifies the version of faceSensor
   
   The explanation of each element in the `msg` block is given below:
   
   `experiment_id` specifies the experiment ID
   
   `source` specifies the source of output message
   
   `sub_type` specifies the sub-type of output message
   
   `timestamp` specifies the time of execution in ISO 8601 format
   
   `trial_id` specifies the trial ID
   
   `version` specifies the version of faceSensor
