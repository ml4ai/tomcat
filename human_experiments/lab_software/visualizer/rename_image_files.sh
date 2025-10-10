#!/bin/bash

source "visualizer_helpers";

exp_dir="/mnt/c/Users/rcham/Documents/ToMCAT/Lab/data/cat/LangLab/dry_runs/group/exp_2023_04_03_13/";
wrk_dir="leopard/screenshots/block_2/";
file_dir="${exp_dir}${wrk_dir}";
filter_types="\.jpg$|\.JPG$|\.png|\.PNG$";

printf "File Dir: ${file_dir}\n";
# exit;

i=0;
readarray -t file_names <<< $(eval ls ${file_dir} | egrep ${filter_types});

# ff=$(sed '${file_names[5]}/_.*//');
# fn=${file_names[5]};
# ff="${fn%%-*}";
# printf "${fn}\n";
# printf "${ff}\n";
# if [[ ${ff} == ?(-)+([0-9]) ]]; then
# 	printf "${ff} is a number!\n";
# else
# 	printf "${ff} is NOT a number!\n"; 
# fi
printf "\n\n\nFile: ${Green}${file_names[0]}${NC}\n";
sleep 4;
ic=0;
for i in "${file_names[@]}"
do
	# printf "\n\n\nFile: ${Green}${i}${NC}\n";
	ff="${i%%_*}";
	# printf "${ff}\n";
	if [[ ${ff} == ?(-)+([0-9]) ]]; then
		# printf "${ff} is a number!\n";
		if [[ ! ${#ff} > 6 ]]; then
			(( ic++ ));
			# printf "${ff} ${Blue}is NOT 6 digits!${NC}\n";
			# nf="000000${ff}";
			# nf6=${nf: -6};
			pre_file_name="${file_dir}${i}";
			new_file_name="${file_dir}${i#*_}";
			clear;
			printf "${BPurple}${wrk_dir}:${NC}\n\n";
			printf "OLD: ${Red}${i}${NC}\n";
			printf "NEW: ${BGreen}${i#*_}  :  ${Green}${ic}${NC}\n";
			# exit;
			mv ${pre_file_name} ${new_file_name};
			# exit;
			# printf "${BBlue}${nf6}${NC} is new number!\n";

		fi
	# else
	# 	printf "${ff} ${Red}is NOT a number!${NC}\n";
	# 	exit; 
	fi
done
printf "\n\nTotal File Names Changed = ${BGreen}${ic}${NC}\n";
