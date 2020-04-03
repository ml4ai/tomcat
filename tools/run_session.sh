#!/bin/bash

github_actions=$GITHUB_ACTIONS

set -u

# Set the TOMCAT environment variable, assuming that the directory structure
# mirrors that of the git repository.
TOMCAT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../" >/dev/null 2>&1 && pwd)"
export TOMCAT


# This function tests whether ffmpeg has access to the webcam. On macOS, the
# terminal must explicitly be given access to the camera and microphone in
# order to run programs that record video and audio.


framerate_option=""
if [[ "$OSTYPE" == "darwin"* ]]; then

    # On macOS, we choose the avfoundation format.
    ffmpeg_fmt=avfoundation
    # On a late 2013 retina MacBook Pro, we have to specify the framerate
    # explicitly for some unknown reason for the webcam recording.
    if [[ $(sysctl hw.model) == "hw.model: MacBookPro11,3" ]]; then
        framerate_option="-framerate 30"
    fi
else
    ffmpeg_fmt=video4linux2
fi

ffmpeg_common_invocation="ffmpeg  -nostdin -f $ffmpeg_fmt"
if [[ "$OSTYPE" == "darwin"* ]]; then
  if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
    terminal="iTerm"
  else
    terminal="Terminal"
  fi
  echo "Testing terminal access to camera."
  mkdir -p /tmp/${USER}/tomcat
  /bin/rm -f /tmp/${USER}/tomcat/test_video.mpg
  ffmpeg -f avfoundation -i "0:" -t 1 /tmp/${USER}/tomcat/test_video.mpg &>/dev/null
  if [[ ! -f /tmp/${USER}/test_video ]]; then
    echo "The terminal does not have access to the webcam."
    ${TOMCAT}/tools/terminal_camera_access.scpt $terminal
    if [[ $? -ne 0 ]]; then exit 1; fi
  fi

  echo "Testing terminal access to microphone..."
  /bin/rm -f /tmp/"$USER"/tomcat/test_audio.wav
  ffmpeg -f avfoundation -i ":0" -t 1 /tmp/"$USER"/tomcat/test_audio.wav &>/dev/null &
  microphone_test_pid=$!
  sleep 1
  if [[ ! -f /tmp/"$USER"/tomcat/test_audio.wav ]]; then
    echo "The terminal does not have access to the microphone."
    kill -9 $microphone_test_pid
    ${TOMCAT}/tools/terminal_microphone_access.scpt $terminal
    if [[ $? -ne 0 ]]; then exit 1; fi
  else
    /bin/rm -f /tmp/"$USER"/tomcat/test_audio.wav
  fi

  exit 1
fi


# this function returns the date and time in the current timezone.
timestamp() {
  date "+%Y_%m_%d_%H_%M_%S"
}

if [[ -n "${github_actions}" ]]; then
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

if ! "${TOMCAT}"/tools/check_minecraft.sh; then exit 1; fi

export tutorial_mission_log="${TOMCAT_TMP_DIR}/tutorial_mission.log"
export zombie_invasion_log="${TOMCAT_TMP_DIR}/zombie_invasion.log"

export num_tries=2

if [[ "$OSTYPE" == "darwin"* ]]; then

  if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
    terminal="iTerm"
  else
    terminal="Terminal"
  fi
  osascript "${TOMCAT}"/tools/activate_minecraft_window.scpt ${terminal}
  if [[ $? -ne 0 ]]; then exit 1; fi
elif [[ "$OSTYPE" == "linux-gnu" ]]; then
  if [[ -z "$GITHUB_ACTIONS" ]]; then
    ${TOMCAT}/tools/activate_minecraft_window.sh
  fi
  if [[ $? -ne 0 ]]; then exit 1; fi
fi

if [[ ${do_tutorial} -eq 1 ]]; then 
  if ! "${TOMCAT}"/tools/run_tutorial.sh; then exit 1; fi
fi

/bin/rm -f "${TOMCAT}"/external/malmo/Minecraft/run/saves/discrete_events/discrete_events.json

if [[ ${do_invasion} -eq 1 ]]; then
    echo " "
    echo "Running the Zombie invasion mission in ${TOMCAT}."
    echo " "

    try=0


    # Creating an output directory for this session.
    output_dir="${TOMCAT}"/data/participant_data/session_$(timestamp)
    mkdir -p "${output_dir}"

    if [[ -z "$github_actions" ]]; then
        echo "Recording video of player's face using webcam."
        ${ffmpeg_common_invocation} ${framerate_option} -i "0:" "${output_dir}"/webcam_video.mpg &> /dev/null &
        webcam_recording_pid=$!
        if [[ $(pgrep ${webcam_recording_pid}) == "" ]]; then exit 1; fi

        echo "Recording player audio using microphone."
        ${ffmpeg_common_invocation} -i ":0" "${output_dir}"/player_audio.wav &> /dev/null &
        audio_recording_pid=$!
    fi

    # Recording game screen.
    ${ffmpeg_common_invocation} -i "1:" -r 30 "${output_dir}"/screen_video.mpg &> /dev/null &
    screen_recording_pid=$!

    while [ $try -lt $num_tries ]; do
        if [[ -n "$github_actions" ]]; then
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

            "${TOMCAT}"/tools/kill_minecraft.sh
            "${TOMCAT}"/tools/check_minecraft.sh
        fi
    done
fi

if [[ -z "$github_actions" ]]; then
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
