#!/bin/bash

input_dir="/space/mlyang721/screenshot-videos"
output_dir="/space/mlyang721/screenshot-videos/no_eyes"
cut_time=280

if [ ! -d "$output_dir" ]; then
  mkdir -p "$output_dir"
fi

for input_file in "$input_dir"/*.mp4; do
  file_name=$(basename "$input_file")
  output_file="$output_dir/$file_name"
  
  ffmpeg -ss $cut_time -i "$input_file" -c copy "$output_file"
  
  echo "save cutting $cut_time into $output_file"
done
