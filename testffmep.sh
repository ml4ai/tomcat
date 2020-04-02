#!/bin/bash
# ffmpeg -f v4l2 -nostdin -i "0:0" video.mpg &> /dev/null &webcam_recording_pid=$!

if [ ! -d "$PWD/ffmData" ]; then 
	mkdir -p ffmData
	echo "folder ffmData has been made"
fi

RECORD_FILE=$PWD/ffmData/videoTest.mpg
if [ -f "$RECORD_FILE" ]; then 
	rm $PWD/ffmData/videoTest.mpg
	echo "file $RECORD_FILE has been removed"
fi

ffmpeg -nostdin -video_size 1024x768 -framerate 25 -f x11grab -i :0.0+100,200 -f \
alsa -ac 2 -i hw:0 $RECORD_FILE & webcam_recording_pid=$!
start=$SECONDS;
X = 0

while true; do
	echo "X = $X"
	sleep 1
	end=$SECONDS;
	dt_sec=$(( end - start ))
	echo "Slept for $dt_sec seconds..."
	ps ax | grep ffmpeg
	if [ "$X" -ge 10 ];
	then
		chmod a+rwx $RECORD_FILE
		echo "PID to kill: $webcam_recording_pid"
		echo "ps ax | grep ffmpeg:"
		ps ax | grep ffmpeg
#		kill -2 $webcam_recording_pid
		kill -15 $webcam_recording_pid
		echo "$webcam_recording_pid is killed"
		echo "Exit"
		break
	fi
	X=$(($X+1))
done
