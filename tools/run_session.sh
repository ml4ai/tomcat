#!/bin/bash

set -u

# this function returns the date and time in the current timezone.
timestamp() {
    date "+%Y_%m_%d_%H_%M_%S"
}

mission_one_time=10
do_tutorial=0
do_invasion=1

# Set the TOMCAT environment variable, assuming that the directory structure
# mirrors that of the git repository.
export TOMCAT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../" >/dev/null 2>&1 && pwd)"
export TOMCAT_TMP_DIR="/tmp/$USER/tomcat"
mkdir -p "${TOMCAT_TMP_DIR}"

${TOMCAT}/tools/check_minecraft.sh
if [[ $? -ne 0 ]]; then exit 1; fi

export tutorial_mission_log="${TOMCAT_TMP_DIR}/tutorial_mission.log"
export zombie_invasion_log="${TOMCAT_TMP_DIR}/zombie_invasion.log"

export num_tries=2

if [[ ${do_tutorial} -eq 1 ]]; then
    ${TOMCAT}/tools/run_tutorial
fi

if [[ ${do_invasion} -eq 1 ]]; then
    echo " "
    echo "Running the Zombie invasion mission in ${TOMCAT}."
    echo " "

    try=0

    framerate_option=""
    if [[ "$OSTYPE" == "darwin"* ]]; then
        read -r x1 y1 width height \
            <<<$(osascript ${TOMCAT}/tools/get_minecraft_window_position_and_size.scpt |
                sed 's/, / /g')

        # On retina displays, we perform the proper scaling.
        if [[ ! -z $(system_profiler SPDisplaysDataType | grep "Retina") ]]; then
            x1=$((2 * x1))
            y1=$((2 * y1))
            width=$((2 * width))
            height=$((2 * height))
        fi
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
        Screen recording currently only works on MacOS since it relies on AppleScript to get
        the position and size of the Minecraft window. Equivalent functionality can
        probably be achieved with the wmctrl tool tool on Linux. Pull requests
        welcome!"
        exit 1
    fi

    # Creating an output directory for this session.
    output_dir=${TOMCAT}/data/participant_data/session_$(timestamp)
    mkdir -p ${output_dir}
    ffmpeg_common_invocation="ffmpeg -nostdin -f $ffmpeg_fmt"

    # Recording video of player's face
    ${ffmpeg_common_invocation} ${framerate_option} \
        -i "0:" ${output_dir}/webcam_video.mpg &>/dev/null &
    webcam_recording_pid=$!

    # Recording player audio
    ${ffmpeg_common_invocation} \
        -i ":0" ${output_dir}/player_audio.wav &>/dev/null &
    audio_recording_pid=$!

    # Recording game screen.
    if [[ "$OSTYPE" == "darwin"* ]]; then
        ${ffmpeg_common_invocation} \
            -i "1:" -vf "crop=$width:$height:$x1:$y1" ${output_dir}/screen_video.mpg \
            &>/dev/null &
        screen_recording_pid=$!
    fi

    while [ $try -lt $num_tries ]; do
        ${TOMCAT}/build/bin/runExperiment \
            --mission 1 \
            --time_limit ${mission_one_time} \
            --record_path "${output_dir}/malmo_data.tgz" \
            &>${zombie_invasion_log} &
        bg_pid=$!
        echo "Running: ${TOMCAT}/build/bin/runExperiment --mission 1"
        echo "    --time_limit ${mission_one_time}"
        echo "    --record_path ${output_dir}/malmo_data.tgz"
        echo "Process corresponding to ./bin/runExperiment is $bg_pid"
        echo "... waiting for it to complete."
        wait $bg_pid
        zombie_invasion_status=$?

        if [[ ${zombie_invasion_status} -eq 0 ]]; then
            zombie_invasion_status=$(grep -c "Error starting mission" \
                ${zombie_invasion_log})
        fi

        if [[ ${zombie_invasion_status} -eq 0 ]]; then
            echo "Zombie invasion mission ended with success status."
            echo " "
            break
        fi

        let try+=1

        if [[ $try -lt $num_tries ]]; then
            echo "Zombie invasion mission ended with failure status."
            echo "Killing all Minecraft and Malmo processes that can be found and trying again."
            ${TOMCAT}/tools/kill_minecraft.sh
            ${TOMCAT}/tools/check_minecraft.sh
        fi
    done
fi

kill -2 $webcam_recording_pid
kill -2 $audio_recording_pid

# For now, screen recording works only on macOS. Need to extend it to Linux as
# well.
if [[ "$OSTYPE" == "darwin"* ]]; then
    kill -2 $screen_recording_pid
fi

echo "Finished running all sessions in ${TOMCAT}."
exit 0
