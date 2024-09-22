#!/bin/bash
base_dir="/space/mlyang721/screenshot-video"
extension="mp4"

for dir in exp_2023_04_17_13 exp_2023_04_18_14 exp_2023_04_21_10 exp_2023_04_24_13 exp_2023_04_27_14 exp_2023_04_28_10 exp_2023_05_01_13 exp_2023_05_02_14 exp_2023_05_03_10; do
	    cd "$base_dir/$dir"
	        for file in *.$extension; do
			        filename=$(basename "$file" .$extension)
				        new_filename="${filename}_h264.$extension"
					        ffmpeg -i "$file" -vcodec libx264 -acodec aac -strict experimental "$new_filename"
						    echo "Converted $file to $new_filename"
						      done
					      done

					      echo "complete all videos"
