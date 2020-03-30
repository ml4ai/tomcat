#!/bin/bash

set -u

declare -x GITHUB_ACTIONS

# this function returns the date and time in the current timezone.
timestamp() {
  date "+%Y_%m_%d_%H_%M_%S"
}

if [[ -n ${GITHUB_ACTIONS} ]]; then
  time_limit=5
  do_tutorial=0
else
  time_limit=600
  do_tutorial=1
fi
do_invasion=1

# Set the TOMCAT environment variable, assuming that the directory structure
# mirrors that of the git repository.
TOMCAT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../" >/dev/null 2>&1 && pwd)"
export TOMCAT

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
  osascript "${TOMCAT}"/tools/activate_minecraft_window.scpt
fi

if [[ ${do_tutorial} -eq 1 ]]; then 
  "${TOMCAT}"/tools/run_tutorial
fi

/bin/rm -f "${TOMCAT}"/external/malmo/Minecraft/run/saves/discrete_events/discrete_events.json

if [[ ${do_invasion} -eq 1 ]]; then
    echo " "
    echo "Running the Zombie invasion mission in ${TOMCAT}."
    echo " "

    try=0

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
        echo "
        Screen recording currently only works on MacOS since it relies on
        AppleScript to bring the Minecraft window to the foreground and make it
        full screen. Equivalent functionality can probably be achieved with
        the wmctrl tool tool on Linux. Pull requests welcome!"
    fi

    # Creating an output directory for this session.
    output_dir="${TOMCAT}"/data/participant_data/session_$(timestamp)
    mkdir -p "${output_dir}"
    ffmpeg_common_invocation="ffmpeg -nostdin -f $ffmpeg_fmt"

    if [[ -z $GITHUB_ACTIONS ]]; then
        # Recording video of player's face
        "${ffmpeg_common_invocation}" "${framerate_option}" \
            -i "0:" "${output_dir}"/webcam_video.mpg &>/dev/null &
        webcam_recording_pid=$!

        # Recording player audio
        "${ffmpeg_common_invocation}" \
            -i ":0" "${output_dir}"/player_audio.wav &>/dev/null &
        audio_recording_pid=$!
    fi

    # Recording game screen.
    if [[ "$OSTYPE" == "darwin"* ]]; then
        "${ffmpeg_common_invocation}" \
            -i "1:" -r 30 "${output_dir}"/screen_video.mpg &> /dev/null &
        screen_recording_pid=$!
    fi

    while [ $try -lt $num_tries ]; do
        if [[ -n "$GITHUB_ACTIONS" ]]; then
            "${TOMCAT}"/build/bin/runExperiment \
            --mission external/malmo/sample_missions/default_flat_1.xml\
            --time_limit ${time_limit} \
            --record_path "${output_dir}"/malmo_data.tgz \
            &>"${zombie_invasion_log}" &
            bg_pid=$!
        else
            "${TOMCAT}"/build/bin/runExperiment \
            --mission 1 \
            --time_limit ${time_limit} \
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
            echo "Zombie invasion mission ended with success status."
            echo "All recorded data is in ${output_dir}"
            echo " "
            break
        fi

        (( try++ ))

        if [[ $try -lt $num_tries ]]; then
            echo "Zombie invasion mission ended with failure status."
            echo "Killing all Minecraft and Malmo processes that can be found"
            echo "and trying again."

            "${TOMCAT}"/tools/kill_minecraft.sh
            "${TOMCAT}"/tools/check_minecraft.sh
        fi
    done
fi

if [[ -z $GITHUB_ACTIONS ]]; then
    kill -2 $webcam_recording_pid
    kill -2 $audio_recording_pid
fi

# For now, screen recording works only on macOS. Need to extend it to Linux as
# well.
if [[ "$OSTYPE" == "darwin"* ]]; then
    kill -2 $screen_recording_pid
fi

# When performing continuous integration with Github Actions, we run a test
# mission that doesn't produce the JSON file corresponding to discrete actions,
# So we only move it in a non-Github Actions environment.
if [[ -z $GITHUB_ACTIONS ]]; then
  mv "${TOMCAT}"/external/malmo/Minecraft/run/saves/discrete_events/discrete_events.json\
     "${output_dir}"/discrete_events.json
fi

echo "Finished running all sessions in ${TOMCAT}".
exit 0
