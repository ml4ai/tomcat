#! /bin/bash

thePath=$1
theFrameRate=$2

ffmpeg -f avfoundation -i 'Capture screen 1' -pix_fmt rgb24 -r $theFrameRate -s 1280x720 -vf "settb=AVTB,setpts='trunc(PTS/1K)*1K+st(1,trunc(RTCTIME/1K))-1K*trunc(ld(1)/1K)', drawtext=text='%{localtime}.%{eif\:1M*t-1K*trunc(t*1K)\:d}':fontsize=54:fontcolor=red@0.9:x=10:y=1400" -strftime 1 "$thePath/%Y-%m-%d_%H-%M-%S.%s.png" &> "$thePath/ffmpeg.log"