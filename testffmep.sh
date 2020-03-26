#!/bin/bash
ffmpeg -f v4l2 -nostdin -i "0:0" video.mpg &> /dev/null &
webcam_recording_pid=$!
sleep 10
kill -2 $webcam_recording_pid
