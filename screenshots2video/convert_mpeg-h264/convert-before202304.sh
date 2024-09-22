#!/bin/bash

for file in *.mp4; do
  filename=$(basename "$file" .mp4)
  ffmpeg -i "$file" -vcodec libx264 -acodec aac -strict experimental "${filename}_h264.mp4"
done

