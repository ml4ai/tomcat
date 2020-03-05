#/usr/bin/bash
#This file by Runnan Zhou and Lize Chen.Feb.20.2020
echo "This will automatically start recording and press Q to quit"
echo "Please enter a name as the output file name"
read output_name

mkdir -p ffmData
VIDEO_FILE=$PWD/ffmData/$output_name.flv
AUDIO_FILE=$PWD/ffmData/$output_name.wav
#x=1

#while [ "$x" -le 10 ];
#    do
    if [ -f "$VIDEO_FILE" ];
    then
        echo "The $VIDEO_FILE is already exist, please choose another file name"
        exit
    else
        ffmpeg -f avfoundation -framerate 30 -i 1:  $PWD/ffmData/$output_name.flv\
         -f avfoundation -i :0 $PWD/ffmData/$output_name.mp3
# Linux user should use this command instead.
#ffmpeg -video_size 1024x768 -framerate 25 -f x11grab -i :0.0+100,200 -f alsa -ac 2 -i
#hw:0 output.mkv
    fi
#    while true;
#        do
#        read -rsn1 input
#        if [ "$input" = "q" ];
#        then
#            exit 0
#        else
#            x=$(($x+1))
#        fi
#    done
#        if [ "$1" == "-a" ];
#         then
#            if [ -f "$AUDIO_FILE" ];
#            then
#                echo "The $AUDIO_FILE is already exist, please choose another file name"
#                exit
#            else
#                ffmpeg -i $PWD/ffmData/$output_name$x.flv -vn -acodec pcm_s16le -ar 16000 -ac 1 -f wav $PWD/ffmData/$output_name.wav
#            fi
#
#
#    fi
#done

