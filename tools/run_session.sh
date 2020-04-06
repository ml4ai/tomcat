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

# On MacBook Pros, it seems that we have to specify the framerate explicitly
# for some unknown reason for the webcam recording.
framerate_option=""
if [[ "$OSTYPE" == "darwin"* ]]; then
  hw_model=$(sysctl hw.model | cut -d' ' -f2)
  if [[ "$hw_model" == "MacBookPro"* ]]; then
      framerate_option="-framerate 30"
  fi
fi

export PATH="$PATH:/opt/local/bin:/opt/local/sbin"

# On macOS, we need to test whether the terminal has access to the webcam and
# microphone.
if [[ "$OSTYPE" == "darwin"* ]]; then
  if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
    terminal="iTerm"
  else
    terminal="Terminal"
  fi
  echo "Testing terminal access to camera."
  mkdir -p /tmp/${USER}/tomcat
  test_video_file=${TOMCAT_TMP_DIR}/test_video.mpg
  $rm -f "$test_video_file" 
  ffmpeg -nostdin -f avfoundation $framerate_option -i "0:" -t 1 "$test_video_file" &> "$TOMCAT_TMP_DIR"/ffmpeg_webcam.log
  if [[ ! -f "$test_video_file" ]]; then
    echo "We were not able to create a test video file, so we assume that"
    echo "macOS is not allowing the terminal to access the camera."
    echo "The script will now guide you to set it up."
    echo ""
    "$tools"/macos/terminal_camera_access.scpt $terminal
    if [[ $? -ne 0 ]]; then exit 1; fi
  fi

  echo "Testing terminal access to microphone..."
  test_audio_file=${TOMCAT_TMP_DIR}/test_audio.wav
  $rm -f "$test_audio_file"
  ffmpeg -nostdin -f avfoundation -i ":0" -t 1 "$test_audio_file" &>"$TOMCAT_TMP_DIR"/ffmpeg_audio.log &
  microphone_test_pid=$!
  sleep 1
  if [[ ! -f "$test_audio_file" ]]; then

    echo "We were not able to create a test audio recording, so we assume that"
    echo "macOS is not allowing the terminal to access the microphone."
    echo "The script will now guide you to set it up."
    echo ""

    # ffmpeg does not exit by itself when you ask it to try to record audio from
    # the default audio input device and permission is denied by macOS. Thus, we
    # have to kill it with signal 9
    kill -9 $microphone_test_pid
    "$tools"/macos/terminal_microphone_access.scpt $terminal
    if [[ $? -ne 0 ]]; then exit 1; fi
  else
    $rm -f "$test_audio_file"
  fi
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
  do_tutorial=0
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
  "$tools"/macos/activate_minecraft_window.scpt ${terminal}
  if [[ $? -ne 0 ]]; then exit 1; fi
elif [[ "$OSTYPE" == "linux-gnu" ]]; then
  # wmctrl does not work well with xvfb-run, so we disable full-screening the
  # Minecraft window when running a headless test of this script with a Github
  # Actions runner.
  if [[ -z "$GITHUB_ACTIONS" ]]; then
    "$tools"/linux/activate_minecraft_window.sh
  fi
  if [[ $? -ne 0 ]]; then exit 1; fi
fi

if [[ ${do_tutorial} -eq 1 ]]; then 
  if ! "$tools"/run_tutorial.sh; then exit 1; fi
fi

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


    # On a Github actions runner, there is no webcam and microphone.
    if [[ -z "$GITHUB_ACTIONS" ]]; then
        echo "Recording video of player's face using webcam."
        if [[ "$OSTYPE" == "darwin"* ]]; then
          fmt=avfoundation
          input_device="0:"

        elif [[ "$OSTYPE" == "linux-gnu" ]]; then
          fmt=vfl2
          input_device=/dev/video0
        fi
        ffmpeg -f ${fmt} ${framerate_option} -i ${input_device}\
          "${output_dir}"/webcam_video.mpg &> /dev/null &
        webcam_recording_pid=$!
        
        echo "Recording player audio using microphone."
        if [[ "$OSTYPE" == "darwin"* ]]; then
          fmt=avfoundation
          input_device=":0"
        elif [[ "$OSTYPE" == "linux-gnu" ]]; then
          fmt=alsa
          input_device=default
        fi
        ffmpeg -nostdin -f ${fmt} -i ${input_device} "${output_dir}"/player_audio.wav &> /dev/null &
        audio_recording_pid=$!
    fi

    # Recording game screen.
    screen_video="${output_dir}"/screen_video.mpg
    if [[ "$OSTYPE" == "darwin"* ]]; then
        ffmpeg -nostdin -f avfoundation -i "1:" -r 30 "$screen_video" &> /dev/null &
        screen_recording_pid=$!
    elif [[ "$OSTYPE" == "linux-gnu" ]]; then
        ffmpeg -nostdin -f x11grab\
          -s $(xdpyinfo | grep dimensions | awk '{print $2;}')\
          -i ":0.0"\
          "$screen_video" &> /dev/null &
        screen_recording_pid=$!
    fi

    while [ $try -lt $num_tries ]; do
        if [[ -n "$GITHUB_ACTIONS" ]]; then
            "${TOMCAT}"/build/bin/runExperiment \
            --mission external/malmo/sample_missions/default_flat_1.xml\
            --time_limit ${time_limit} \
            --record_observations \
            > "${output_dir}"/malmo_data.json \
            &>"${zombie_invasion_log}" &
            bg_pid=$!
        else
            "${TOMCAT}"/build/bin/runExperiment \
            --mission 1 \
            --time_limit ${time_limit} \
            --record_observations \
            > "${output_dir}"/malmo_data.json \
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
    kill $audio_recording_pid
    kill $screen_recording_pid
fi

discrete_actions_file="${TOMCAT}"/external/malmo/Minecraft/run/saves/discrete_events/discrete_events.json

if [[ -f "${discrete_actions_file}" ]]; then
  mv "${discrete_actions_file}" "${output_dir}"/discrete_events.json
fi

# Move the self-reports from the Minecraft folder to the participant data folder
mv "${TOMCAT}"/external/malmo/Minecraft/run/saves/self_reports/* "${output_dir}"

echo "Finished running all sessions in ${TOMCAT}".
exit 0
