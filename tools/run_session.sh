#!/bin/bash

github_actions="$GITHUB_ACTIONS"
set -u
export GITHUB_ACTIONS="${github_actions}"

# Set the TOMCAT environment variable, assuming that the directory structure
# mirrors that of the git repository.
TOMCAT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../" >/dev/null 2>&1 && pwd)"
export TOMCAT

export TOMCAT_TMP_DIR="/tmp/$USER/tomcat"
mkdir -p "${TOMCAT_TMP_DIR}"

# On some systems, the 'rm' command is aliased to something else. So we take
# the precaution of explicitly pointing rm to /bin/rm.
rm=/bin/rm

tools="$TOMCAT"/tools

# A function to handle cleanup when a user interrupts the script with Ctrl+C
user_interrupt() {
  echo "Detected keyboard interrupt."
  echo "Cleaning up now"
  "$tools"/kill_minecraft.sh
  if [[ "$OSTYPE"  == "darwin"* ]]; then
    # Switching the audio output from the multi-output device to the built-in
    # output.
    if ! SwitchAudioSource -s "Built-in Output"; then exit 1; fi
  fi
  exit
}

trap user_interrupt SIGINT

framerate_option=""

export PATH="$PATH:/opt/local/bin:/opt/local/sbin"

test_system_audio_recording_macos() {
  blackhole_is_setup=$(ffmpeg -f avfoundation -list_devices true -i "" 2>&1 | grep "\[0\] BlackHole 16ch")
  if [[ $blackhole_is_setup == "" ]]; then
    echo "BlackHole system audio recording virtual output device is not set up."
    echo "We will do it now."
    if ! "$tools"/activate_system_audio_recording.scpt; then exit 1; fi
  fi

  # Switching audio output to multi-output device so that we can record system
  # audio.
  if ! SwitchAudioSource -s "Multi-Output Device"; then exit 1; fi
}

test_webcam_macos() {
  if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
    terminal="iTerm"
  else
    terminal="Terminal"
  fi
  mkdir -p /tmp/${USER}/tomcat
  test_video_file=${TOMCAT_TMP_DIR}/test_video.mpg
  $rm -f "$test_video_file" 
  ffmpeg_webcam_log="$TOMCAT_TMP_DIR"/ffmpeg_webcam.log
  ffmpeg -nostdin -f avfoundation $framerate_option -i "0:" -t 0.1 "$test_video_file" &> "$ffmpeg_webcam_log"
  if [[ ! -f "$test_video_file" ]]; then
    camera_access_err=$(cat "$ffmpeg_webcam_log" \
      | grep "Failed to create AV capture input device: Cannot use FaceTime HD Camera")
    framerate_err=$(cat "$ffmpeg_webcam_log" \
      | grep "Selected framerate (29.970030) is not supported by the device")
    if [[ ! $camera_access_err == "" ]]; then
      echo "Caught camera access error:" $camera_access_err
      echo "macOS is not allowing the terminal to access the camera."
      echo "The script will now guide you to set it up."
      if ! "$tools"/terminal_camera_access.scpt $terminal; then exit 1; fi
      test_webcam_macos
    elif [[ ! $framerate_err == "" ]]; then
      # On recent MacBook Pros, it seems that we have to specify the framerate
      # explicitly for some unknown reason for the webcam recording.
      echo "Caught unsupported framerate error:" $framerate_err
      echo "We will now set the framerate to 30 going forward."
      framerate_option="-framerate 30"
      test_webcam_macos
    else
      echo "Unable to capture webcam video - unhandled ffmpeg error."
      echo "The complete error log is given below."
      echo ""
      cat "$ffmpeg_webcam_log"
      exit 1
    fi
  fi
}

test_microphone_macos() {
  test_microphone_file=${TOMCAT_TMP_DIR}/test_microphone.wav
  $rm -f "$test_microphone_file"
  ffmpeg_microphone_log="$TOMCAT_TMP_DIR"/ffmpeg_microphone.log
  ffmpeg -nostdin -f avfoundation -i ":1" -t 1 "$test_microphone_file" &> "$ffmpeg_microphone_log" &
  microphone_test_pid=$!
  sleep 1
  if [[ ! -f "$test_microphone_file" ]]; then

    microphone_access_err=$(cat "$ffmpeg_microphone_log" \
      | grep "Failed to create AV capture input device: Cannot use Built-in Microphone")

    if [[ ! $microphone_access_err == "" ]]; then
      echo "macOS is not allowing the terminal to access the microphone."
      echo "The script will now guide you to set it up."
      echo ""

      # ffmpeg does not exit by itself when you ask it to try to record audio
      # from the default audio input device and permission is denied by macOS.
      # Thus, we have to kill it with signal 9.
      kill -9 $microphone_test_pid
      if ! "$tools"/terminal_microphone_access.scpt $terminal; then exit 1; fi
      test_microphone_macos
    else
      echo "Unable to capture microphone audio - unhandled ffmpeg error."
      echo "The complete error log is given below."
      echo ""
      cat "$ffmpeg_microphone_log"
      exit 1
    fi
  fi
}

test_screen_recording_macos() {
  test_screen_video_file=${TOMCAT_TMP_DIR}/test_screen_video.mpg
  ffmpeg_screen_video_log="$TOMCAT_TMP_DIR"/screen_video_recording.log
  dimensions=$(xdpyinfo | grep dimensions | awk '{print $2;}')
  $rm -f "$test_screen_video_file"
  ffmpeg -nostdin -f avfoundation -i "1:" -t 1 -s $dimensions "$test_screen_video_file" >& "$ffmpeg_screen_video_log" &
  sleep 1
  # Check if the test video file exists
  if [[ ! -f "$test_screen_video_file" || ! $(du -sh "$ffmpeg_screen_video_log" | grep "0B") == ""  ]]; then
    echo "Unable to capture screen video: unhandled ffmpeg error."
    echo "The complete error log is given below."
    echo ""
    cat "$ffmpeg_screen_video_log"
    exit 1
  fi
}

# On macOS, we need to test whether the terminal has access to the webcam and
# microphone.
if [[ "$OSTYPE" == "darwin"* ]]; then
  echo "Testing terminal access to camera."
  test_webcam_macos
  echo "Checking if system audio recording is set up..."
  test_system_audio_recording_macos
  echo "Testing terminal access to microphone..."
  test_microphone_macos
  echo "Testing screen capture..."
  test_screen_recording_macos
fi


# this function returns the date and time in the current timezone.
timestamp() {
  date "+%Y_%m_%d_%H_%M_%S"
}

if [[ -n "${GITHUB_ACTIONS}" ]]; then
  time_limit=1
  do_tutorial=0
else
  time_limit=600
  do_tutorial=1
fi
do_invasion=1


export TOMCAT_TMP_DIR="/tmp/$USER/tomcat"
mkdir -p "${TOMCAT_TMP_DIR}"

# Trying to set the correct version of Java.
macports_found=`[ -x "$(command -v port)" ]; echo $?`
if [[ $macports_found -eq 0 ]]; then
  export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk8/Contents/Home
fi

if ! "$tools"/check_minecraft.sh; then exit 1; fi

export tutorial_mission_log="${TOMCAT_TMP_DIR}/tutorial_mission.log"
export zombie_invasion_log="${TOMCAT_TMP_DIR}/zombie_invasion.log"

export num_tries=2

if [[ "$OSTYPE" == "darwin"* ]]; then
  if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
    terminal="iTerm"
  else
    terminal="Terminal"
  fi
  "$tools"/activate_minecraft_window.scpt ${terminal}
  if [[ $? -ne 0 ]]; then exit 1; fi
elif [[ "$OSTYPE" == "linux-gnu" ]]; then
  # wmctrl does not work well with xvfb-run, so we disable full-screening the
  # Minecraft window when running a headless test of this script with a Github
  # Actions runner.
  if [[ -z "$GITHUB_ACTIONS" ]]; then
    "$tools"/activate_minecraft_window.sh
  fi
  if [[ $? -ne 0 ]]; then exit 1; fi
fi

if [[ ${do_tutorial} -eq 1 ]]; then 
  if ! "$tools"/run_tutorial.sh; then exit 1; fi
fi

sleep 6

# Get rid of any pre-existing discrete events JSON files.
$rm -f "${TOMCAT}"/external/malmo/Minecraft/run/saves/discrete_events/discrete_events.json

if [[ ${do_invasion} -eq 1 ]]; then
    echo " "
    echo "Running the Zombie invasion mission in ${TOMCAT}."
    echo " "

    try=0

    # Creating an output directory for this session.
    output_dir="${TOMCAT}"/data/participant_data/session_$(timestamp)
    mkdir -p "${output_dir}"


    # On a Github actions runner, there is no webcam, microphone or speaker.
    if [[ -z "$GITHUB_ACTIONS" ]]; then
        echo "Recording video of player's face using webcam."
        if [[ "$OSTYPE" == "darwin"* ]]; then
          fmt=avfoundation
          input_device="0:"

        elif [[ "$OSTYPE" == "linux-gnu" ]]; then
          fmt=v4l2
          input_device=/dev/video0
        fi
        ffmpeg -f ${fmt} ${framerate_option} -i ${input_device}\
          "${output_dir}"/webcam_video.mpg &> /dev/null &
        webcam_recording_pid=$!
        
        echo "Recording player audio using microphone."
        if [[ "$OSTYPE" == "darwin"* ]]; then
          fmt=avfoundation
          # The default input microphone would normally be ":0", but since we
          # have installed BlackHole to record system audio, the input device
          # corresponding to the microphone is ":1".
          input_device=":1"
        elif [[ "$OSTYPE" == "linux-gnu" ]]; then
          fmt=alsa
          input_device=default
        fi
        ffmpeg -nostdin -f ${fmt} -i ${input_device} "${output_dir}"/player_audio.wav &> /dev/null &
        microphone_recording_pid=$!

        if [[ "$OSTYPE" == "darwin"* ]]; then
          echo "Recording system audio."
          fmt=avfoundation
          input_device=":0"
          ffmpeg -nostdin -f ${fmt} -i ${input_device} \
            "${output_dir}"/system_audio.wav &> "$TOMCAT_TMP_DIR"/system_audio_recording.log &
          system_audio_recording_pid=$!
        elif [[ "$OSTYPE" == "linux-gnu" ]]; then
          echo "System audio recording not yet enabled for Linux."
        fi

    fi

    # Recording game screen.
    screen_video="${output_dir}"/screen_video.mpg
    dimensions=$(xdpyinfo | grep dimensions | awk '{print $2;}')
    if [[ "$OSTYPE" == "darwin"* ]]; then
        fmt=avfoundation
        input_device="1:"
    elif [[ "$OSTYPE" == "linux-gnu" ]]; then
        fmt=x11grab
        input_device=":0.0"
    fi
    ffmpeg -nostdin -f $fmt\
      -i $input_device\
      -s $dimensions\
      -r 30\
      "$screen_video" >& "$TOMCAT_TMP_DIR"/screen_video_recording.log &
    screen_recording_pid=$!

    while [ $try -lt $num_tries ]; do
        if [[ -n "$GITHUB_ACTIONS" ]]; then
            "${TOMCAT}"/build/bin/runExperiment \
            --mission external/malmo/sample_missions/default_flat_1.xml\
            --time_limit ${time_limit} \
            --record_observations \
            --record_path "${output_dir}"/malmo_data.tgz \
            &>"${zombie_invasion_log}" &
            bg_pid=$!
        else
            "${TOMCAT}"/build/bin/runExperiment \
            --mission 1 \
            --time_limit ${time_limit} \
            --record_observations \
            --record_path "${output_dir}"/malmo_data.tgz \
            &>"${zombie_invasion_log}" &
            bg_pid=$!
        fi

        echo "Running: ${TOMCAT}/build/bin/runExperiment --mission 1"
        echo "    --time_limit ${time_limit}"
        echo "    --record_path ${output_dir}/malmo_data.tgz"
        echo "Process corresponding to ./bin/runExperiment is $bg_pid"
        echo "... waiting for it to complete."
        wait $bg_pid
        zombie_invasion_status=$?

        if [[ ${zombie_invasion_status} -eq 0 ]]; then
            zombie_invasion_status=$(grep -c "Error starting mission" \
            "${zombie_invasion_log}")
        fi

        if [[ ${zombie_invasion_status} -eq 0 ]]; then
            echo "Zombie invasion mission ended with exit code 0."
            echo "All recorded data is in ${output_dir}"
            echo " "
            break
        fi

        (( try++ ))

        if [[ $try -lt $num_tries ]]; then
            echo "Zombie invasion mission ended with exit code 1."
            echo "Killing all Minecraft and Malmo processes that can be found"
            echo "and trying again."

            "$tools"/kill_minecraft.sh
            "$tools"/check_minecraft.sh
        fi
    done
fi

if [[ -z "$GITHUB_ACTIONS" ]]; then
  kill $webcam_recording_pid
  kill $microphone_recording_pid
  kill $screen_recording_pid
  if [[ "$OSTYPE"  == "darwin"* ]]; then
    kill $system_audio_recording_pid
    # Switching the audio output from the multi-output device to the built-in
    # output.
    if ! SwitchAudioSource -s "Built-in Output"; then exit 1; fi
  fi
fi


discrete_actions_file="${TOMCAT}"/external/malmo/Minecraft/run/saves/discrete_events/discrete_events.json

if [[ -f "${discrete_actions_file}" ]]; then
  mv "${discrete_actions_file}" "${output_dir}"/discrete_events.json
fi

# Move the self-reports from the Minecraft folder to the participant data folder
mv "${TOMCAT}"/external/malmo/Minecraft/run/saves/self_reports/* "${output_dir}"

echo "Finished running all sessions in ${TOMCAT}".
exit 0
