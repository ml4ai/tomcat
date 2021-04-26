Usage
=====

To run an experimental session, run the following command from the `tomcat` directory.

    ./tools/run_session
    
By default, the above command runs the tutorial mission followed by the hand-crafted Zombie mission. To run the procedurally generated version of the Zombie mission (which is still in its early stages of development), run the following command from the `tomcat` directory.

    MAIN_MISSION=3 ./tools/run_session


The data from the session will be saved in the folder:

    tomcat/data/participant_data/<session_id>

where `<session_id>` is a UUID.

This folder will contain the following files:
- `metadata.json`: A JSON file containing metadata about the session.
- Folders corresponding to the missions run in the session, of the form
  `mission_N`, where `N` is some identifier (e.g. `mission_0`, `mission_1`,
  etc.).
- Each folder `mission_N` will have the following files:
    - `webcam_video.mpg` : A video recording of the player's face taken using the built-in
    webcam.
    - `screen_video.mpg` : A video recording of the Minecraft playthrough (i.e.
    what the player sees on their screen)
    - `player_audio.wav` : A WAV file containing the audio recording of the player.
    - `system_audio.wav` : A WAV file containing a recording of the sound
      output produced by the computer. This will not be produced on macOS
      computers running macOS Mojave or older.
    - `messages.txt` : A file containing timestamped observations of the player's
                     state, in-game events, and self-report responses. Each
                     line in this file corresponds to a JSON object.

The format for the event and observation data can be found
[here](https://ml4ai.github.io/tomcat/tomcat_openapi).

To interrupt the game and quit at any time, press the `Esc` key, then click on
the terminal window where you ran the `run_session.sh` script, and then
interrupt the process with `Ctrl+C`. You can also quit the game by closing the
Minecraft window.
