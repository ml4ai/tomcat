#! /bin/bash

set -u

out_dir=$1
frame_rate=$2

# We source from zprofile so that a call to this script from a non-interactive shell recognizes ffmpeg.
# ffmpeg was installed with MacPorts. The PATH variable is updated in ~/.zprofile by MacPorts.
source "$HOME/.zprofile"

CONFIG="settb=AVTB,setpts='trunc(PTS/1K)*1K+st(1,trunc(RTCTIME/1K))-1K*trunc(ld(1)/1K)', "
CONFIG+="drawtext=text='%{localtime}.%{eif\:1M*t-1K*trunc(t*1K)\:d}':fontsize=54:fontcolor=red@0.9:x=10:y=1400"
RECORDINGS_FILEPATH="$out_dir/%Y-%m-%d_%H-%M-%S.%s.png"
LOG_FILEPATH="$out_dir/ffmpeg.log"

ffmpeg -f avfoundation -i 'Capture screen 1' -pix_fmt rgb24 -r "$frame_rate" -s 1280x720 -vf "$CONFIG" -strftime 1 "$RECORDINGS_FILEPATH" &> "$LOG_FILEPATH"