#! /bin/bash

set -u

out_dir=$1
frame_rate=$2

# We source from zprofile so that a call to this script from a non-interactive shell recognizes ffmpeg.
# ffmpeg was installed with MacPorts. The PATH variable is updated in ~/.zprofile by MacPorts.
source "$HOME/.zprofile"

RECORDINGS_FILEPATH="$out_dir/output%06d.png"
LOG_FILEPATH="$out_dir/ffmpeg.log"

ffmpeg -f avfoundation -framerate "$frame_rate" -s 1280x720 -i 'FaceTime HD Camera' -vf fps="$frame_rate" "$RECORDINGS_FILEPATH" &> "$LOG_FILEPATH"
