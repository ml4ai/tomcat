#!/bin/bash

# Application: Copy Pre-session data for a Particapant to the Experiment directory as Symbolic Links.
# Decription: Scans the Pre-session directories and copy the data for a Particapant into the correct Animal under the Experiment data
#             as Symbolic Links. The program uses the input file "participant_ids.csv" (var participant_ids_file)
#             to list the Experiments and the Particapants ID for each Animal. It will create a directory "presession" under the
#             Experiment/Animal/ directory if one does not exist. The program will create a master log file "combine_presession_data.log"
#             that records all Symbolic Links created for presession files found for each Experiment/Animal/Participant ID.
#             The program also creates a log file in the the presession directory "symlinks_created_in_exp_yyyy_mm_dd_hh_for_xxxx.log"
#             that records the Symbolic Links that were created in the Experiment Directory.  
#             
# By: Rick Champlin
# Last Updated: 7/13/2023
# Start: Run Bash Script "combine_presession_data.sh".
# Dependency files: "participant_ids.csv"


my_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Color Vars:
  NC="\033[0m"              # Text Reset

  #Regular Colors
  Black='\033[0;30m'        # Black
  Red='\033[0;31m'          # Red
  Green='\033[0;32m'        # Green
  Yellow='\033[0;33m'       # Yellow
  Blue='\033[0;34m'         # Blue
  Purple='\033[0;35m'       # Purple
  Cyan='\033[0;36m'         # Cyan
  White='\033[0;37m'        # White
  Orange='\033[38;5;214m'   # Orange

  # Bold
  BBlack='\033[1;30m'       # Black
  BRed='\033[1;31m'         # Red
  BGreen='\033[1;32m'       # Green
  BYellow='\033[1;33m'      # Yellow
  BBlue='\033[1;34m'        # Blue
  BPurple='\033[1;35m'      # Purple
  BCyan='\033[1;36m'        # Cyan
  BWhite='\033[1;37m'       # White
# End - Color Vars



# On i03:
pre_root_dir="/tomcat/data/raw/LangLab/experiments/study_3_pilot/presession/";
exp_root_dir="/tomcat/data/raw/LangLab/experiments/study_3_pilot/group/";
participant_ids_file="participant_ids.csv";
log_file="${pre_root_dir}combine_presession_data.log";

participants="lion tiger leopard";
eval "part_arr=(${participants})";


log_line(){
  # Create and make enteries into glodal log file "combine_presession_data.log".
  log_loc="${log_file}";
  if [[ "${1}" == "RESET" ]]; then
    if test -f "${log_loc}" ; then
      sudo rm -f "${log_loc}";
    fi

  else
    if [ ! -f "${log_loc}" ]; then
      sudo touch "${log_loc}";
      sudo chmod 777 "${log_loc}";
    fi
    sudo printf "${1}" >> "${log_loc}";
  fi
}


log_in_presession(){
  # Create and make enteries into Presession directory log file "symlinks_created_in_exp_yyyy_mm_dd_hh_for_xxxx.log".
  pre_dir=$(echo "${pre_arr[0]}" | sed 's![^/]*$!!');
  pre_log_file="${pre_dir}symlinks_created_in_${exp_arr[2]}_for_${pd}.log"
  if [[ "${1}" == "RESET" ]]; then
    if test -f "${pre_log_file}" ; then
      sudo rm -f "${pre_log_file}";
    fi

  else
    if [ ! -f "${pre_log_file}" ]; then
      sudo touch "${pre_log_file}";
      sudo chmod 777 "${pre_log_file}";
    fi
    sudo printf "${1}" >> "${pre_log_file}";
  fi
}



# Create new global log file.
log_line "RESET";

while read exp_line; do
  # Read in next line from "participant_ids.csv" and read into "exp_arr" array var.
  IFS=',' read -r -a exp_arr <<< "${exp_line}";
  # Check to see if line is Header or Blank, if so, continue to end of loop.
  if [[ "${exp_arr[2]}" == "Experiment ID" || "${exp_arr[2]}" == "" ]]; then continue; fi
  
  clear;
  dt=$(date +%Y-%m-%d_%H:%M:%S);
  printf "${dt}  ${BBlue}EXP: ${BRed}${exp_arr[2]}    ${Blue}Lion ID: ${BGreen}${exp_arr[3]}    ${Blue}Tiger ID: ${BGreen}${exp_arr[4]}    ${Blue}Leopard ID: ${BGreen}${exp_arr[5]}${NC}\n";
  log_line "${dt}  EXP: ${exp_arr[2]}    Lion ID: ${exp_arr[3]}    Tiger ID: ${exp_arr[4]}    Leopard ID: ${exp_arr[5]}\n";
  if [[ "${exp_arr[1]}" == "CANCELED" ]]; then
    printf "                          ${BYellow}*** Experiment Was Canceled ***${NC}\n\n\n";
    log_line "                          *** Experiment Was Canceled ***\n\n\n";
  else
    for (( pi=0; pi<3; pi++ )); do # Loop for each Animal (lion, tiger, leopard).
      # Setup varibles and search for Presession Files.
      let "li = $pi + 3 ";
      pl="${part_arr[${pi}]^}";
      pd="${exp_arr[${li}]//$'\r'/''}";
      eval "pre_arr=($(ls ${pre_root_dir}**/* | grep "${pd}" | grep -v ".log"))";
      pc=${#pre_arr[@]};

      # Make presession directory under Experiment/Animal.
      px="${exp_root_dir}${exp_arr[2]}/${part_arr[${pi}]}/presession/";
      if ! test -d "${px}" ; then
        # presession directory does not exist, create it.
        sudo mkdir -p "${px}";
      else
        # presession directory already exist, delete any files in it.
        cd "${px}";
        sudo rm -f *;
        cd "${my_dir}";
      fi
    
      if (( ${pc} > 0 )); then
        # Presession files found for Participant ID.
        printf "                          ${Cyan}${pl} ID: ${Green}${pd} - ${BPurple}${pc} ${Cyan}Presession Files Found, Created Symbolic Links:${NC}\n";
        log_line "                          ${pl} ID: ${pd} - ${pc} Presession Files Found, Created Symbolic Links:\n";

        log_in_presession "RESET";
        log_in_presession "${dt}  Created Symlink EXP: ${exp_arr[2]} Animal: ${part_arr[${pi}]}  For ID: ${pd}\n";

        # Loop for each Presession File found.
        for (( ps=0; ps<"${pc}"; ps++ )); do
          # px="${exp_root_dir}${exp_arr[2]}/${part_arr[${pi}]}/presession/";
          # if ! test -d "${px}" ; then sudo mkdir -p "${px}"; fi
          if [[ ${ps} = 0 ]]; then
            # cd "${px}";
            # sudo rm -f *;
            # cd "${my_dir}";
            printf "                             ${BYellow}${px}${NC}\n";
            log_line "                             ${px}\n";
          fi

          # Create Symlink to Presession File in Experiment/Animal/Presession directory.
          sudo ln -s "${pre_arr[${ps}]}" "${px}";

          log_in_presession "      Exp Dir: ${px}\n";
          log_in_presession "      Link To: ${pre_arr[${ps}]}\n\n";
          printf "                                ${BPurple}${pre_arr[${ps}]}${NC}\n";
          log_line "                                ${pre_arr[${ps}]}\n";

        done

      else
        # No Presession files found for Participant ID. May be Experimenter.
        pn="${px}no_presession_files_found_for_id_${pd}.log";
        sudo touch "${pn}";

        printf "                          ${Cyan}${pl} ID: ${Green}${pd} - ${Yellow}(No Presession Files Found!)${NC}\n";
        log_line "                          ${pl} ID: ${pd} - (No Presession Files Found!)\n";
        printf "                             ${BYellow}${px}${NC}\n";
        log_line "                             ${px}\n";
        printf "                                ${BPurple}${pn}${NC}\n";
        log_line "                                ${pn}\n";

      fi
      log_line "\n\n";

    done
    printf "\n\n";
    sleep 6;

  fi
done < "${participant_ids_file}"

printf "\n\n************ DONE WITH ALL EXPERIMENTS ************\n";
log_line "\n\n************ DONE WITH ALL EXPERIMENTS ************\n";


exit;
