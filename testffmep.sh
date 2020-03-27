#!/bin/bash
ffmpeg -f v4l2 -nostdin -i "0:0" video.mpg &> /dev/null &
webcam_recording_pid=$!
start=$SECONDS;
X = 0

while true; do
	echo "X = $X"
	sleep 1
	end=$SECONDS;
	dt_sec=$(( end - start ))
	echo "Slept for $dt_sec seconds..."
	ps ax | grep ffmpeg
	if [ "$X" -ge 5 ];
	then
		echo "PID to kill: $webcam_recording_pid"
		echo "ps ax | grep ffmpeg:"
		ps ax | grep ffmpeg
		kill -2 $webcam_recording_pid
		echo "$webcam_recording_pid is killed"
		echo "Exit"
		break
	fi
	X=$(($X+1))
done
