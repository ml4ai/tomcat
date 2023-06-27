#!/bin/bash

# Application: View Face and Screen Images from Experiments to record analysis of the images.
# Decription: View Face and Screen Images for an Experiment, per Participant, at the beginning of the Experiment + 2 minutes,
#             Hands on Training + 30 seconds, Saturn A + 30 seconds, and Saturn B + 30 seconds.
#             These images are showing to the user to make determinations of: is the participant an experimenter,
#             does the participant have a mask on, does the participant have a EEG/NIRS cap on,
#             does the participant have Pupil Tracking glasses on,
#             and is there a mission start time/end time (no mission start time means mission was not initiated in experiment).
#             The program provides a menu toolbar at the bottom of images with hot keys to quickly input the analysis of the images.
# By: Rick Champlin
# Last Updated: 6/13/2023
# Prerequisites: The program uses "imgcat" to be able to view images on "iTerm" while being SSH into server.
#                Must install "imgcat" by running "pip install imgcat".
# Start: Run Bash Script "view_exp_face_screen.sh".
# Support Files: "mission.csv" Start and end times for Minecraft mission for each Experiment. 
#                "participant_ids.csv" Participant / Experimenter IDs for each Experiment.
#                "experimenter_ids.csv" Table of "sit-in" Experimenters and the IDs assigned to them.
# Output Files: "view_exp_face_screen_all_crosstab.csv" The analysis data fields for each experiment are listed as individual columns.
#               "view_exp_face_screen_all_stacked.csv"  The analysis data fields for each experiment are stacked as rows.


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

  # Background
  On_Black='\033[40m'       # Black
  On_Red='\033[41m'         # Red
  On_Green='\033[42m'       # Green
  On_Yellow='\033[43m'      # Yellow
  On_Blue='\033[44m'        # Blue
  On_Purple='\033[45m'      # Purple
  On_Cyan='\033[46m'        # Cyan
  On_White='\033[47m'       # White




# On gauss:
in_root_dir="/tomcat/data/raw/LangLab/experiments/study_3_pilot/group/";
in_mission_file="/space/rchamplin/view_exp_face_screen/mission.csv";
in_participant_ids_file="/space/rchamplin/view_exp_face_screen/participant_ids.csv";

# All Experiments:
experiments="exp_2022_09_30_10 exp_2022_10_04_09 exp_2022_10_07_15 exp_2022_10_14_10 exp_2022_10_18_10 exp_2022_10_21_15 exp_2022_10_24_12 exp_2022_10_27_10 exp_2022_10_28_10 exp_2022_10_31_10 exp_2022_11_01_10 exp_2022_11_04_10 exp_2022_11_07_10 exp_2022_11_08_11 exp_2022_11_10_10 exp_2022_11_14_12 exp_2022_11_15_13 exp_2022_11_17_15 exp_2022_11_18_10 exp_2022_11_22_10 exp_2022_12_02_15 exp_2022_12_05_12 exp_2023_01_30_13 exp_2023_01_31_14 exp_2023_02_03_10 exp_2023_02_06_13 exp_2023_02_07_14 exp_2023_02_10_10 exp_2023_02_16_14 exp_2023_02_20_01 exp_2023_02_20_13 exp_2023_02_21_14 exp_2023_04_17_13 exp_2023_04_18_14 exp_2023_04_20_14 exp_2023_04_21_10 exp_2023_04_24_13 exp_2023_04_27_14 exp_2023_04_28_10
exp_2023_05_01_13 exp_2023_05_02_14 exp_2023_05_03_10";
out_file="view_exp_face_screen_all_crosstab.csv";
out_file_stacked="view_exp_face_screen_all_stacked.csv";

# Experiments with a experimenter sitting-in:
# experiments="exp_2022_09_30_10 exp_2022_10_24_12 exp_2022_10_27_10 exp_2022_10_28_10 exp_2022_10_31_10 exp_2022_11_01_10 exp_2022_11_04_10 exp_2022_11_07_10 exp_2022_12_02_15 exp_2022_12_05_12 exp_2023_01_30_13 exp_2023_02_06_13 exp_2023_02_07_14 exp_2023_02_20_01 exp_2023_04_20_14 exp_2023_04_21_10 exp_2023_04_27_14 exp_2023_05_01_13 exp_2023_05_02_14 exp_2023_05_03_10";
# out_file="view_exp_face_screen_test_img_types_crosstab.csv";
# out_file_stacked="view_exp_face_screen_test_img_types_stacked.csv";


participants="lion tiger leopard";

cap_types="eeg nirs";

gls_types="pupil";

tasks="Start Hands-on Saturn_A Saturn_B";

log_tasks="rest_state finger_tapping affective_task_individual affective_task_team ping_pong_competetive_0 ping_pong_competetive_1 ping_pong_cooperative_0 hands_on_training saturn_a saturn_b";

baseline_tasks="rest_state finger_tapping affective_task_individual affective_task_team ping_pong_competetive_0 ping_pong_competetive_1 ping_pong_cooperative_0 hands_on_training";

face_dir="face_images/";
scrn_dir="screenshots/";

face_start="output000500.png";
scrn_start="output000500.png";

stacked_columns="experiment_id data_name lion tiger leopard";
stacked_rows="subject_id mask_status eeg_data_rest_state eeg_data_finger_tapping eeg_data_affective_task_individual eeg_data_affective_task_team eeg_data_ping_pong_competetive_0 eeg_data_ping_pong_competetive_1 eeg_data_ping_pong_cooperative_0 eeg_data_hands_on_training eeg_data_saturn_a eeg_data_saturn_b nirs_data_rest_state nirs_data_finger_tapping nirs_data_affective_task_individual nirs_data_affective_task_team nirs_data_ping_pong_competetive_0 nirs_data_ping_pong_competetive_1 nirs_data_ping_pong_cooperative_0 nirs_data_hands_on_training nirs_data_saturn_a nirs_data_saturn_b pupil_data_rest_state pupil_data_finger_tapping pupil_data_affective_task_individual pupil_data_affective_task_team pupil_data_ping_pong_competetive_0 pupil_data_ping_pong_competetive_1 pupil_data_ping_pong_cooperative_0 pupil_data_hands_on_training pupil_data_saturn_a pupil_data_saturn_b";

fields="experiment_id lion_subject_id tiger_subject_id leopard_subject_id lion_mask_status tiger_mask_status leopard_mask_status lion_eeg_data_rest_state lion_eeg_data_finger_tapping lion_eeg_data_affective_task_individual lion_eeg_data_affective_task_team lion_eeg_data_ping_pong_competetive_0 lion_eeg_data_ping_pong_competetive_1 lion_eeg_data_ping_pong_cooperative_0 lion_eeg_data_hands_on_training lion_eeg_data_saturn_a lion_eeg_data_saturn_b tiger_eeg_data_rest_state tiger_eeg_data_finger_tapping tiger_eeg_data_affective_task_individual tiger_eeg_data_affective_task_team tiger_eeg_data_ping_pong_competetive_0 tiger_eeg_data_ping_pong_competetive_1 tiger_eeg_data_ping_pong_cooperative_0 tiger_eeg_data_hands_on_training tiger_eeg_data_saturn_a tiger_eeg_data_saturn_b leopard_eeg_data_rest_state leopard_eeg_data_finger_tapping leopard_eeg_data_affective_task_individual leopard_eeg_data_affective_task_team leopard_eeg_data_ping_pong_competetive_0 leopard_eeg_data_ping_pong_competetive_1 leopard_eeg_data_ping_pong_cooperative_0 leopard_eeg_data_hands_on_training leopard_eeg_data_saturn_a leopard_eeg_data_saturn_b lion_nirs_data_rest_state lion_nirs_data_finger_tapping lion_nirs_data_affective_task_individual lion_nirs_data_affective_task_team lion_nirs_data_ping_pong_competetive_0 lion_nirs_data_ping_pong_competetive_1 lion_nirs_data_ping_pong_cooperative_0 lion_nirs_data_hands_on_training lion_nirs_data_saturn_a lion_nirs_data_saturn_b tiger_nirs_data_rest_state tiger_nirs_data_finger_tapping tiger_nirs_data_affective_task_individual tiger_nirs_data_affective_task_team tiger_nirs_data_ping_pong_competetive_0 tiger_nirs_data_ping_pong_competetive_1 tiger_nirs_data_ping_pong_cooperative_0 tiger_nirs_data_hands_on_training tiger_nirs_data_saturn_a tiger_nirs_data_saturn_b leopard_nirs_data_rest_state leopard_nirs_data_finger_tapping leopard_nirs_data_affective_task_individual leopard_nirs_data_affective_task_team leopard_nirs_data_ping_pong_competetive_0 leopard_nirs_data_ping_pong_competetive_1 leopard_nirs_data_ping_pong_cooperative_0 leopard_nirs_data_hands_on_training leopard_nirs_data_saturn_a leopard_nirs_data_saturn_b lion_pupil_data_rest_state lion_pupil_data_finger_tapping lion_pupil_data_affective_task_individual lion_pupil_data_affective_task_team lion_pupil_data_ping_pong_competetive_0 lion_pupil_data_ping_pong_competetive_1 lion_pupil_data_ping_pong_cooperative_0 lion_pupil_data_hands_on_training lion_pupil_data_saturn_a lion_pupil_data_saturn_b tiger_pupil_data_rest_state tiger_pupil_data_finger_tapping tiger_pupil_data_affective_task_individual tiger_pupil_data_affective_task_team tiger_pupil_data_ping_pong_competetive_0 tiger_pupil_data_ping_pong_competetive_1 tiger_pupil_data_ping_pong_cooperative_0 tiger_pupil_data_hands_on_training tiger_pupil_data_saturn_a tiger_pupil_data_saturn_b leopard_pupil_data_rest_state leopard_pupil_data_finger_tapping leopard_pupil_data_affective_task_individual leopard_pupil_data_affective_task_team leopard_pupil_data_ping_pong_competetive_0 leopard_pupil_data_ping_pong_competetive_1 leopard_pupil_data_ping_pong_cooperative_0 leopard_pupil_data_hands_on_training leopard_pupil_data_saturn_a leopard_pupil_data_saturn_b";

# Toolbar vars:
options="1 2 3 f s x y c g";
oc_length="169";                  # Toolbar Display Length (in character displayed on terminal)
oc_off="${BWhite}${On_Black}";    # Toolbar Option Not Selected - Color
oc_red="${Black}${On_Red}";       # Toolbar Option Selected on Red - Color
oc_yellow="${Black}${On_Yellow}"; # Toolbar Option Selected on Yellow - Color
oc_green="${Black}${On_Green}";   # Toolbar Option Selected on Green - Color
oc0="${NC}${Blue}|${NC}";         # Toolbar Option's Divider String - Color
ocb="${Red}";                     # Toolbar Back Option - Color
ocn="${Green}";                   # Toolbar Next Option - Color

# Convert variable space delimeted lists into arrays: 
eval "exp_arr=(${experiments})";
eval "part_arr=(${participants})";
eval "cap_types_arr=(${cap_types})";
eval "gls_types_arr=(${gls_types})";
eval "task_arr=(${tasks})";
eval "log_tasks_arr=(${log_tasks})";
eval "baseline_tasks_arr=(${baseline_tasks})";
eval "field_arr=(${fields})";
eval "option_arr=(${options})";
eval "stacked_column_arr=(${stacked_columns})";
eval "stacked_row_arr=(${stacked_rows})";



log_data() {
  # For TESTING:
  # printf "\nlog_data - field (arg 1): ${1}      reason (arg 2): ${2}\n";
  # sleep 2;

  if [[ "${1}" == "RESET" ]]; then
    if test -f "${out_file}" ; then
      rm "${out_file}";
    fi
    if test -f "${out_file_stacked}" ; then
      rm "${out_file_stacked}";
    fi
    return 0;
  fi

  if [[ "${1}" == "HEADER" ]]; then
    line_out="";
    for (( i=0; i<"${#field_arr[@]}"; i++ )); do
      if (( $(( ${i} + 1 )) < ${#field_arr[@]} )); then
        line_out="${line_out}${field_arr[${i}]},";
      else
        line_out="${line_out}${field_arr[${i}]}\n";
      fi
    done
    printf "${line_out}" > "${out_file}";

    line_out="";
    for (( i=0; i<"${#stacked_column_arr[@]}"; i++ )); do
      if (( $(( ${i} + 1 )) < ${#stacked_column_arr[@]} )); then
        line_out="${line_out}${stacked_column_arr[${i}]},";
      else
        line_out="${line_out}${stacked_column_arr[${i}]}\n";
      fi
    done
    printf "${line_out}" > "${out_file_stacked}";

    return 0;
  fi

  if [[ "${1}" == "WRITE" ]]; then
    # Write to out_file (crosstab):
    declare ${field_arr[0]}="${exp_id}";
    line_out="";
    for (( i=0; i<"${#field_arr[@]}"; i++ )); do
      fld_name="${field_arr[${i}]}";
      fld_data="${!fld_name}";

      # For TESTING:
      # printf "Write out - ${fld_name}: ${fld_data}\n"

      if (( $(( ${i} + 1 )) < ${#field_arr[@]} )); then
        line_out="${line_out}|${fld_data}|,";
      else

        # For TESTING:
        # printf "${i} of ${#field_arr[@]} : ${field_arr[${i}]}\n";

        line_out="${line_out}|${fld_data}|\n";
      fi

      # For TESTING:
      # printf "${i} line_out: ${line_out}\n";
    done
    printf "${line_out//|/\"}" >> "${out_file}";


    # Write to out_file_stacked:
    line_out="";
    for (( rs=0; rs<"${#stacked_row_arr[@]}"; rs++ )); do
      rs_name="${stacked_row_arr[${rs}]}";
      for (( cs=0; cs<"${#stacked_column_arr[@]}"; cs++ )); do
        cs_name="${stacked_column_arr[${cs}]}";
        if [[ "${cs}" == "0" ]]; then line_out="|${exp_id}|,"; continue; fi
        if [[ "${cs}" == "1" ]]; then line_out="${line_out}|${rs_name}|,"; continue; fi
        fld_name="${cs_name}_${rs_name}";
        fld_data="${!fld_name}";
        if (( $(( ${cs} + 1 )) < ${#stacked_column_arr[@]} )); then line_out="${line_out}|${fld_data}|,"; else line_out="${line_out}|${fld_data}|\n"; fi
      done
      printf "${line_out//|/\"}" >> "${out_file_stacked}";
    done
    printf "\n" >> "${out_file_stacked}";

    return 0;
  fi

  if [[ "${1}" == "CLEAR" ]]; then
    for (( i=0; i<"${#field_arr[@]}"; i++ )); do
      eval "${field_arr[${i}]}"='';
    done
    return 0;
  fi

  # NOTES:
  # log_data "${part_id}_${data_type_id}_data_${baseline_task_id}" "${1}"
  # FOR TESTING:
  # printf "Field Name: ${1}    Data: ${2}\n";
  for (( i=0; i<"${#field_arr[@]}"; i++ )); do
    if [[ "${1}" == "${field_arr[${i}]}" ]]; then
      eval "${field_arr[${i}]}"='${2}';
      break;
    fi
  done
}


clear_option_color() {
  for option_id in "${option_arr[@]}"; do
    eval "oc${option_id}"='${oc_off}';
  done
}


clear_tool_bar() {
  printf "\r";
  for (( ci=1; ci<="${1}"; ci++ )); do
    printf " ";
  done
  printf "\r";
}


clear_part_task_fields() {
  if [[ "${task_id}" == "Start" ]]; then
    clr_list=$(printf -- "%s\n" "${field_arr[@]}" | grep "${part_id}" | grep -v "hands\|saturn\|mask");
  else
    # NOTES:
    # lion_eeg_data_hands_on_training lion_eeg_data_saturn_a lion_eeg_data_saturn_b
    if [[ "${task_id}" == "Hands-on" ]]; then clr_task="hands_on_training"; fi
    if [[ "${task_id}" == "Saturn_A" ]]; then clr_task="saturn_a"; fi
    if [[ "${task_id}" == "Saturn_B" ]]; then clr_task="saturn_b"; fi
    clr_list=$(printf -- "%s\n" "${field_arr[@]}" | grep "${part_id}" | grep "${clr_task}");
  fi
  clr_list="${clr_list//$'\n'/' '}";
  for clr_fld in ${clr_list}; do
    eval "${clr_fld}"='';
    # FOR TESTING:
    # printf "Cleared Field: ${clr_fld}\n";
  done
}


get_options_set() {
  options_set="";
  for option_id in "${option_arr[@]}"; do
    oc_sn="oc${option_id}";
    if [[ "${!oc_sn}" != "${oc_off}" ]]; then options_set="${options_set}${option_id} "; fi
  done
}


toggle_reason() {
  oc_name="oc${3}";
  if [[ "${!oc_name}" == "${oc_off}" ]]; then
    reason="${1}";
    eval "${oc_name}"='${4}';
  else
    reason="";
    eval "${oc_name}"='${oc_off}';
  fi
}


log_reason() {
  # NOTES:
  # ${1}=task_id (Start, Hands-on, Saturn_A, Saturn_B, Mask),   ${2}=data_types_group (cap, glasses, mask),  ${3}=reason ("ok", "on", "") 
  # FOR TESTING:
  # printf "\nlog_reason IN - task_id: ${1}   Task group: ${2}   Reason: ${3}\n"

  case ${2} in
    cap)
      eval "data_types_arr=(${cap_types})";
    ;;
    glasses)
      eval "data_types_arr=(${gls_types})";
    ;;
  esac

  case ${1} in
    Start)
      for data_type_id in "${data_types_arr[@]}"; do
        for baseline_task_id in "${baseline_tasks_arr[@]}"; do
          # FOR TESTING:
          # printf "log_reason Start Task Loop - Field: ${part_id}_${data_type_id}_data_${baseline_task_id}    Reason: ${3}\n";
          log_data "${part_id}_${data_type_id}_data_${baseline_task_id}" "${3}"
        done
      done
    ;;
    Hands-on) 
      for data_type_id in "${data_types_arr[@]}"; do
        log_data "${part_id}_${data_type_id}_data_hands_on_training" "${3}"
      done
    ;;
    Saturn_A) 
      for data_type_id in "${data_types_arr[@]}"; do
        log_data "${part_id}_${data_type_id}_data_saturn_a" "${3}"
      done
    ;;
    Saturn_B) 
      for data_type_id in "${data_types_arr[@]}"; do
        log_data "${part_id}_${data_type_id}_data_saturn_b" "${3}"
      done
    ;;
    Mask) 
      log_data "${part_id}_mask_status" "${3}"
    ;;
  esac
}


record_check() {
  # NOTES:
  # tasks="Start Hands-on Saturn_A Saturn_B";
  # Call log_reason - ${1}=task_id (Start, Hands-on, Saturn_A, Saturn_B, Mask),   ${2}=data_types_group (cap, glasses, mask),  ${3}=reason ("ok", "on", "") 
  # FOR TESTING:
  # printf "record_check - arg 1: ${1}\n";
  # printf "1=Exp-No_Cap|2=Exp-No_Gls|6=Part-No_Face|7=Part-No_Scrn|x=Part-No_Cap|y=Part-No_Gls|c=Part-Cap_On|g=Part-Gls_On|n=Next-Pic";

  case $1 in
    1) # Experimenter, No Cap On.
      toggle_reason "experimenter_no_cap_on" "all" "${1}" "${oc_red}";
      log_reason "${task_id}" "cap" "${reason}";
    ;;
  
    2) # Experimenter, No Glasses On.
      toggle_reason "experimenter_no_glasses_on" "all" "${1}" "${oc_red}";
      log_reason "${task_id}" "glasses" "${reason}";
    ;;

    3) # No Minecraft Mission Start Time Found, per Mission.
      if [[ "${task_id}" != "Start" ]]; then
        toggle_reason "misson_start_time_not_found" "${task_id}" "${1}" "${oc_red}";
        log_reason "${task_id}" "cap" "${reason}";
        log_reason "${task_id}" "glasses" "${reason}";
      else
        clear_tool_bar "${oc_length}";
        printf "\r${BRed}** Not on Minecraft Mission Task **${NC}";
        sleep 3;
      fi
    ;;

    f) # Participant, No Face Image, per Task.
      toggle_reason "no_face_image" "${task_id}" "${1}" "${oc_red}";
      log_reason "${task_id}" "cap" "${reason}";
      log_reason "${task_id}" "glasses" "${reason}";
    ;;

    s) # Participant, No Screen Image, per Task.
      toggle_reason "no_screen_image" "${task_id}" "${1}" "${oc_red}";
      log_reason "${task_id}" "cap" "${reason}";
      log_reason "${task_id}" "glasses" "${reason}";
    ;;

    x) # Participant, No Cap On, per task
      toggle_reason "participant_no_cap_on" "${task_id}" "${1}" "${oc_red}";
      log_reason "${task_id}" "cap" "${reason}";
    ;;
  
    y) # Participant, No Glasses On, per task
      toggle_reason "participant_no_glasses_on" "${task_id}" "${1}" "${oc_red}";
      log_reason "${task_id}" "glasses" "${reason}";
    ;;

    c) # Participant or Experimenter, per task -  Cap Is On
      toggle_reason "ok" "${task_id}" "${1}" "${oc_green}";
      log_reason "${task_id}" "cap" "${reason}";
    ;;
  
    g) # Participant or Experimenter, per task -  Glasses Are On
      toggle_reason "ok" "${task_id}" "${1}" "${oc_green}";
      log_reason "${task_id}" "glasses" "${reason}";
    ;;

    m) # Participant or Experimenter, per task -  Has Mask On
      toggle_reason "mask_on" "Mask" "${1}" "${oc_yellow}";
      log_reason "Mask" "mask" "${reason}";
    ;;

    o) # Participant, per task -  Cap Is On and Glasses Are On
      toggle_reason "ok" "${task_id}" "c" "${oc_green}";
      log_reason "${task_id}" "cap" "${reason}";
      toggle_reason "ok" "${task_id}" "g" "${oc_green}";
      log_reason "${task_id}" "glasses" "${reason}";
    ;;

    " ") # Skip to Next Task
        get_options_set;
        if [[ "${options_set}" == "" ]]; then
          printf "\007"; sleep .2; printf "\007";
          clear_tool_bar "${oc_length}";
          printf "${Yellow}** You have not made an analysis selection **   ${BYellow}Do you want still want to skip to Next Task?${oc0}${BWhite}n=Continue with this Task${oc0}${ocn}y=Skip to Next-Task >${NC}";
          read -s -N 1 skip_input;
          if [[ "${skip_input}" == "y" ]]; then ch_rept=false; printf "\n"; fi
        else
          ch_rept=false; printf "\n";
        fi
    ;;

    b) # Step Back Task
      if [[ ${task_ix} == 0 ]]; then
        if [[ ${part_ix} == 0 ]]; then
          printf "\007"; sleep .2; printf "\007";
          clear_tool_bar "${oc_length}";
          printf "\r${Red}*** Cannot Backup Any More - At Begining Particapant and Task of Experiment ***${NC}";
          sleep 5;
          ch_rept=true;
        else
          (( part_ix-- ));
          part_id="${part_arr[${part_ix}]}";
          task_ix="${#task_arr[@]}";
          (( task_ix-- )); (( task_ix-- ));
          ch_rept=false; printf "\n";
        fi
      else
        (( task_ix-- )); (( task_ix-- ));
        ch_rept=false; printf "\n";
      fi
    ;;

    0) # Experiment Canceled
      printf "\007"; sleep .2; printf "\007";
      clear_tool_bar "${oc_length}";
      printf "${Yellow}** Selected CANCEL **  ${BYellow}Confirm Experiment: ${exp_id} - 'Canceled'?${oc0}${BWhite}n=Abort${oc0}${ocn}y=Mark Canceled and Step to Next-Experiment >${NC}";
      read -s -N 1 skip_input;
      if [[ "${skip_input}" == "y" ]]; then
        lion_subject_bk="${lion_subject_id}";
        tiger_subject_bk="${tiger_subject_id}";
        leopard_subject_bk="${leopard_subject_id}";
        log_data "CLEAR";
        lion_subject_id="${lion_subject_bk} - (experiment_canceled)";
        tiger_subject_id="${tiger_subject_bk} - (experiment_canceled)";
        leopard_subject_id="${leopard_subject_bk} - (experiment_canceled)";
        task_ix=999;
        part_ix=999;
        ch_rept=false; printf "\n";
      fi
    ;;


    *)
      clear_tool_bar "${oc_length}"; 
      printf "\r${BRed}** Not a valid option **${NC}";
      sleep 2;
    ;;
  esac
}


input_checks() {
  ch_rept=true;
  while ( ${ch_rept} )
  do
    # option_color
    printf "${ocb}<b=Back-Task${oc0}0=Cancel${oc0}${oc1}1=Exp-No_Cap${oc0}${oc2}2=Exp-No_Gls${oc0}${oc3}3=No_Mission_Start${oc0}${ocf}f=Part-No_Face${oc0}${ocs}s=Part-No_Scrn${oc0}${ocx}x=Part-No_Cap${oc0}${ocy}y=Part-No_Gls${oc0}${occ}c=Cap_On${oc0}${ocg}g=Gls_On${oc0}${ocm}m=Mask_On${oc0}${ocn}space=Next-Task>${NC}";
    read -s -N 1 ch_input;
    record_check "${ch_input}";
    if ( ${ch_rept} ); then  clear_tool_bar "${oc_length}"; fi
  done
}


get_participant_ids() {
  ids_line="";
  lion_subject_id="";
  tiger_subject_id="";
  leopard_subject_id="";
  cur_id="";
  cur_exp=false;
  ids_line=$(grep "${exp_id}" "${in_participant_ids_file}");
  if [[ "${ids_line}" != "" ]]; then
    IFS=',' read -r -a ids_line_arr <<< "${ids_line}";
    # clr_list="${clr_list//$'\n'/' '}";
    lion_subject_id="${ids_line_arr[3]//$'\r'/''}";
    tiger_subject_id="${ids_line_arr[4]//$'\r'/''}";
    leopard_subject_id="${ids_line_arr[5]//$'\r'/''}";
    id_name="${part_id}_subject_id";
    cur_id="${!id_name}";
    if [[ "${cur_id: 0: 3}" == "999" ]]; then cur_exp=true; else cur_exp=false; fi
  fi
}


get_mission_start_time() {
  task_line="";
  zdt="";
  sdtu="";
  task_date="";
  task_dt7="";
  task_line=$(grep "${exp_id},${task_id}" "${in_mission_file}");
  # printf "\ntask_line: ${task_line}\n";

  if [[ "${task_line}" != "" ]]; then
    IFS=',' read -r -a task_line_arr <<< "${task_line}";
    tdt="${task_line_arr[4]}";
    sdt=$(date -d "${tdt: -24: 10} ${tdt: -13: 8}" "+%Y-%m-%d %H:%M:%S");
    zdt="$( date -d "$(date -Iseconds -d  "${sdt}") -7 hours 0 minutes 0 seconds" "+%Y-%m-%d %H:%M:%S")";

    tdtu="${task_line_arr[5]}";
    sdtu=$(date -d "@${tdtu}" "+%Y-%m-%d %H:%M:%S");
    # zdtu="$( date -d "$(date -Iseconds -d  "${sdtu}") -7 hours 0 minutes 0 seconds" "+%Y-%m-%d %H:%M:%S")"; # No adjustment for time zone needed with UNIX Time

    # For TESTING:
    # printf "\nTask Start Time - EXP: ${exp_id}  FOR: ${part_id}  TASK: ${task_id}   ISO8601: ${zdt}   UNIX: ${sdtu}\n";
    # sleep 3;

    # task_date="${sdtu: 0: 16}"; # Down to minutes.
    task_date="${sdtu: 0: 19}"; # Include seconds. 
    # sdtu format="2022-10-21 16:57:52"
    # task_date format="2022-10-21 16:57:52"
    task_dt7="$( date -d "$(date -Iseconds -d  "${task_date}") 7 hours 0 minutes 0 seconds" "+%Y-%m-%d %H:%M:%S")"
    # task_dt7 format="2022-10-21 23:57:52"
  fi
}


get_image_files() {
  face_name="";
  scrn_name="";
  face_dir="";
  scrn_dir="";
  face_fdir="";
  scrn_fdir="";
  face_file="";
  scrn_file="";
  task_date="";

  if [[ "${task_id}" == "Start" ]]; then
    # For "Start" (Baseline) Images:
    if [[ "${exp_id: 4: 10}" < "2022_11_07" || "${exp_id: 4: 10}" == "2023_01_30" || "${exp_id: 4: 10}" == "2023_01_31" ]]; then
    # if [[ "${exp_id: 4: 10}" < "2023_02_03" ]]; then
      # Old Image Name Format: output000500.png
      face_dir="face_images/";
      scrn_dir="screenshots/";
      face_fdir="${in_dir}${part_id}/${face_dir}";
      scrn_fdir="${in_dir}${part_id}/${scrn_dir}";

      face_name="${face_start}";
      scrn_name="${scrn_start}";

      face_file="${in_dir}${part_id}/${face_dir}${face_name}";
      scrn_file="${in_dir}${part_id}/${scrn_dir}${scrn_name}";

      task_date="Start_Exp";

      return 0;
    fi

    if [[ "${exp_id: 4: 10}" < "2023_04_17" ]]; then
      # Timestamp Image Name Format Without Sequence Number: 2023-02-07T23_58_09.472672446Z.png
      # No Blocks
      face_dir="face_images/";
      scrn_dir="screenshots/";
      face_fdir="${in_dir}${part_id}/${face_dir}";
      scrn_fdir="${in_dir}${part_id}/${scrn_dir}";

      src_str="${exp_id: 4: 10}";
      src_str="${src_str//_/-}";
      
      ffn=$(ls "${face_fdir}" | grep "${src_str}" | head -1);                                             # Get First Image Name > "ffn".
      dt1="${ffn: 0: 19}";                                                                                # Get Date and Time off the front of File Name > "dt1".
      dt1="${dt1//_/:}";                                                                                  # Convert "dt1" into Bash DateTime>  "dt1".
      dt2="$( date -d "$(date -Iseconds -d  "${dt1}") 0 hours 2 minutes 0 seconds" "+%Y-%m-%d %H:%M:%S")" # Add 2 Minutes to "dt1" > "dt2".
      dt2="${dt2// /T}";                                                                                  # Put "T" back into middle > "dt2".
      src_new="${dt2//:/_}";                                                                              # Replace ":" with "_" > "src_new".
      src_new="${src_new: 0: 16}";                                                                        # Cut down to Minutes for search "2023-02-07T23_58" > "src_new".
      face_name=$(ls "${face_fdir}" | grep "${src_new}" | head -1);                                       # Find File Name of file 2 minutes later > "face_name".
      
      ffn=$(ls "${scrn_fdir}" | grep "${src_str}" | head -1);                                             # Get First Image Name > "ffn".
      dt1="${ffn: 0: 19}";                                                                                # Get Date and Time off the front of File Name > "dt1".
      dt1="${dt1//_/:}";                                                                                  # Convert "dt1" into Bash DateTime>  "dt1".
      dt2="$( date -d "$(date -Iseconds -d  "${dt1}") 0 hours 2 minutes 0 seconds" "+%Y-%m-%d %H:%M:%S")" # Add 2 Minutes to "dt1" > "dt2".
      dt2="${dt2// /T}";                                                                                  # Put "T" back into middle > "dt2".
      src_new="${dt2//:/_}";                                                                              # Replace ":" with "_" > "src_new".
      src_new="${src_new: 0: 16}";                                                                        # Cut down to Minutes for search "2023-02-07T23_58" > "src_new".
      scrn_name=$(ls "${scrn_fdir}" | grep "${src_new}" | head -1);                                       # Find File Name of file 2 minutes later > "scrn_name".
  
    else
      # Timestamp Image Name Format With Sequence Number: 020704_2023-04-17_21-59-22.439.PM~157.png
      # With Blocks 
      face_dir="face_images/block_1/";
      scrn_dir="screenshots/block_1/";
      face_fdir="${in_dir}${part_id}/${face_dir}";
      scrn_fdir="${in_dir}${part_id}/${scrn_dir}";

      src_str="${exp_id: 4: 10}";
      src_str="${src_str//_/-}";
      
      ffn=$(ls "${face_fdir}" | grep "${src_str}" | head -1); # Get First Image Name > "ffn".
      dtd="${ffn: 7: 10}";                                    # Get Date off the File Name > "dtd".
      dtt="${ffn: 18: 8}";                                    # Get Time off the File Name > "dtt".
      dtt="${dtt//-/:}";                                      # Replace "-" with ":" in Time >  "dtt".
      # Add 2 Minutes to "${dtd}T${dtt}" > "dt2": 
      dt2="$( date -d "$(date -Iseconds -d  "${dtd}T${dtt}") 0 hours 2 minutes 0 seconds" "+%Y-%m-%d %H:%M:%S")";
      # dt2="2023-02-06 22:55:49"
      dt2="${dt2// /_}";                                            # Put "_" back into middle > "dt2".
      src_new="${dt2//:/-}";                                        # Replace ":" with "-" > "src_new".
      src_new="${src_new: 0: 16}";                                  # Cut down to Minutes for search "2023-02-07_23-58" > "src_new".
      face_name=$(ls "${face_fdir}" | grep "${src_new}" | head -1); # Find File Name of file 2 minutes later > "face_name".
      
      ffn=$(ls "${scrn_fdir}" | grep "${src_str}" | head -1); # Get First Image Name > "ffn".
      dtd="${ffn: 7: 10}";                                    # Get Date off the File Name > "dtd".
      dtt="${ffn: 18: 8}";                                    # Get Time off the File Name > "dtt".
      dtt="${dtt//-/:}";                                      # Replace "-" with ":" in Time >  "dtt".
      # Add 2 Minutes to "${dtd}T${dtt}" > "dt2": 
      dt2="$( date -d "$(date -Iseconds -d  "${dtd}T${dtt}") 0 hours 2 minutes 0 seconds" "+%Y-%m-%d %H:%M:%S")";
      # dt2="2023-02-06 22:55:49"
      dt2="${dt2// /_}";                                            # Put "_" back into middle > "dt2".
      src_new="${dt2//:/-}";                                        # Replace ":" with "-" > "src_new".
      src_new="${src_new: 0: 16}";                                  # Cut down to Minutes for search "2023-02-07_23-58" > "src_new".
      scrn_name=$(ls "${scrn_fdir}" | grep "${src_new}" | head -1); # Find File Name of file 2 minutes later > "scrn_name".

    fi
    task_date="Start_Exp";


  else
    # For "Mission" (Minecraft) Images:

    get_mission_start_time
    # task_date format="2022-10-21 16:57:52"
    if [[ "${task_date}" == "" ]]; then return 0; fi

    if [[ "${exp_id: 4: 10}" < "2022_11_07" || "${exp_id: 4: 10}" == "2023_01_30" || "${exp_id: 4: 10}" == "2023_01_31" ]]; then
    # if [[ "${exp_id: 4: 10}" < "2023_02_03" ]]; then
      # Old Image Name Format: output000500.png
      face_dir="face_images/";
      scrn_dir="screenshots/";
      face_fdir="${in_dir}${part_id}/${face_dir}";
      scrn_fdir="${in_dir}${part_id}/${scrn_dir}";
 
      face_name="";
      file_line="";
      # unset line_arr;
      # -rw-r--r-- 1 root root  671606 2022-09-30 13:09:13.698767910 -0700 output063601.png
      file_line=$(ls --full-time "${face_fdir}" | grep "${task_date}" | head -1); # ****************************** Maybe wrong task date!
      # file_line="${file_line//" "/","}";
      # IFS=',' read -r -a line_arr <<< "${file_line}";
      # face_name="${line_arr[9]}"
      if [[ "${file_line}" != "" ]]; then face_name="${file_line: -16: 16}"; fi

      scrn_name="";
      file_line="";
      # unset line_arr;
      # -rw-r--r-- 1 root root  934543 2022-09-30 13:08:57.450100667 -0700 output056565.png
      file_line=$(ls --full-time "${scrn_fdir}" | grep "${task_date}" | head -1); # ****************************** Maybe wrong task date!
      # file_line="${file_line//" "/","}";
      # IFS=',' read -r -a line_arr <<< "${file_line}";
      # scrn_name="${line_arr[8]}"
      if [[ "${file_line}" != "" ]]; then scrn_name="${file_line: -16: 16}"; fi

      if [[ "${face_name}" != "" ]]; then face_file="${in_dir}${part_id}/${face_dir}${face_name}"; fi
      if [[ "${scrn_name}" != "" ]]; then scrn_file="${in_dir}${part_id}/${scrn_dir}${scrn_name}"; fi

      return 0;
    fi

    if [[ "${exp_id: 4: 10}" < "2023_04_17" ]]; then
      # Timestamp Image Name Format Without Sequence Number: 2023-02-07T23_58_09.472672446Z.png
      # task_date format="2022-10-21 16:57:52"
      # No Blocks
      face_dir="face_images/";
      scrn_dir="screenshots/";
      face_fdir="${in_dir}${part_id}/${face_dir}";
      scrn_fdir="${in_dir}${part_id}/${scrn_dir}";
      
      dt2="${task_dt7// /T}";     # Put "T" back into middle > "dt2".
      src_str="${dt2//:/_}";       # Replace ":" with "_" > "src_new".
      src_str="${src_str: 0: 19}"; # Cut down to Seconds for search "2023-02-07T23_58_09" > "src_new".

      ffn=$(ls "${face_fdir}" | grep "${src_str}" | head -1);                                             # Get First Image Name that matches Mission Start Date/Time > "ffn".
      dt1="${ffn: 0: 19}";                                                                                # Get Date and Time off the front of File Name > "dt1".
      dt1="${dt1//_/:}";                                                                                  # Convert "dt1" into Bash DateTime>  "dt1".
      dt2="$( date -d "$(date -Iseconds -d  "${dt1}") 0 hours 0 minutes 30 seconds" "+%Y-%m-%d %H:%M:%S")" # Add 30 Seconds to "dt1" > "dt2".
      dt2="${dt2// /T}";                                                                                  # Put "T" back into middle > "dt2".
      src_new="${dt2//:/_}";                                                                              # Replace ":" with "_" > "src_new".
      src_new="${src_new: 0: 19}";                                                                        # Cut down to Second for search "2023-02-07T23_58_24" > "src_new".
      face_name=$(ls "${face_fdir}" | grep "${src_new}" | head -1);                                       # Find File Name of file 30 Seconds later > "face_name".
  
      ffn=$(ls "${scrn_fdir}" | grep "${src_str}" | head -1);                                             # Get First Image Name that matches Mission Start Date/Time > "ffn".
      dt1="${ffn: 0: 19}";                                                                                # Get Date and Time off the front of File Name > "dt1".
      dt1="${dt1//_/:}";                                                                                  # Convert "dt1" into Bash DateTime>  "dt1".
      dt2="$( date -d "$(date -Iseconds -d  "${dt1}") 0 hours 0 minutes 30 seconds" "+%Y-%m-%d %H:%M:%S")" # Add 30 Seconds to "dt1" > "dt2".
      dt2="${dt2// /T}";                                                                                  # Put "T" back into middle > "dt2".
      src_new="${dt2//:/_}";                                                                              # Replace ":" with "_" > "src_new".
      src_new="${src_new: 0: 19}";                                                                        # Cut down to Second for search "2023-02-07T23_58_24" > "src_new".
      scrn_name=$(ls "${scrn_fdir}" | grep "${src_new}" | head -1);                                       # Find File Name of file 30 Seconds later > "scrn_name".

    else
      # Timestamp Image Name Format With Sequence Number: 020704_2023-04-17_21-59-22.439.PM~157.png
      # With Blocks 
      face_dir="face_images/block_2/";
      scrn_dir="screenshots/block_2/";
      face_fdir="${in_dir}${part_id}/${face_dir}";
      scrn_fdir="${in_dir}${part_id}/${scrn_dir}";

      # printf "\nGet Images - task_date: ${task_date}   task_dt7: ${task_dt7}\n";

      dt2="${task_dt7// /_}";     # Put "T" back into middle > "dt2".
      src_str="${dt2//:/-}";       # Replace ":" with "_" > "src_new".
      src_str="${src_str: 0: 19}"; # Cut down to Seconds for search "2023-02-07T23_58_09" > "src_new".

      # printf "\nGet Images - src_str: ${src_str}\n";


      ffn=$(ls "${face_fdir}" | grep "${src_str}" | head -1); # Get First Image Name > "ffn".

      # printf "\nGet Images - ffn: ${ffn}\n";

      dtd="${ffn: 7: 10}";                                   # Get Date off the File Name > "dtd".
      dtt="${ffn: 18: 8}";                                   # Get Time off the File Name > "dtt".
      dtt="${dtt//-/:}";                                     # Replace "-" with ":" in Time >  "dtt".
      # Add 30 Seconds to "${dtd}T${dtt}" > "dt2": 
      dt2="$( date -d "$(date -Iseconds -d  "${dtd}T${dtt}") 0 hours 0 minutes 30 seconds" "+%Y-%m-%d %H:%M:%S")";
      # dt2="2023-02-06 22:55:49"
      dt2="${dt2// /_}";                                           # Put "_" back into middle > "dt2".
      src_new="${dt2//:/-}";                                       # Replace ":" with "-" > "src_new".
      src_new="${src_new: 0: 19}";                                 # Cut down to Second for search "2023-02-07_23-58-24" > "src_new".
      face_name=$(ls "${face_fdir}" | grep "${src_new}" | head -1); # Find File Name of file 30 Seconds later > "face_name".
      
      ffn=$(ls "${scrn_fdir}" | grep "${src_str}" | head -1); # Get First Image Name > "ffn".
      dtd="${ffn: 7: 10}";                                   # Get Date off the File Name > "dtd".
      dtt="${ffn: 18: 8}";                                   # Get Time off the File Name > "dtt".
      dtt="${dtt//-/:}";                                     # Replace "-" with ":" in Time >  "dtt".
      # Add 30 Seconds to "${dtd}T${dtt}" > "dt2": 
      dt2="$( date -d "$(date -Iseconds -d  "${dtd}T${dtt}") 0 hours 0 minutes 30 seconds" "+%Y-%m-%d %H:%M:%S")";
      # dt2="2023-02-06 22:55:49"
      dt2="${dt2// /_}";                                           # Put "_" back into middle > "dt2".
      src_new="${dt2//:/-}";                                       # Replace ":" with "-" > "src_new".
      src_new="${src_new: 0: 19}";                                 # Cut down to Second for search "2023-02-07_23-58-24" > "src_new".
      scrn_name=$(ls "${scrn_fdir}" | grep "${src_new}" | head -1); # Find File Name of file 30 Seconds later > "scrn_name".

    fi
  fi
  
  if [[ "${face_name}" != "" ]]; then face_file="${in_dir}${part_id}/${face_dir}${face_name}"; fi
  if [[ "${scrn_name}" != "" ]]; then scrn_file="${in_dir}${part_id}/${scrn_dir}${scrn_name}"; fi
}




# *** Main Loop ***

log_data "RESET";
log_data "HEADER";

for exp_id in "${exp_arr[@]}"; do
  
  in_dir="${in_root_dir}${exp_id}/";

  log_data "CLEAR";

  for (( part_ix=0; part_ix<"${#part_arr[@]}"; part_ix++ )); do
    part_id="${part_arr[${part_ix}]}";
    eval "ocm"='${oc_off}';

    for (( task_ix=0; task_ix<"${#task_arr[@]}"; task_ix++ )); do
      task_id="${task_arr[${task_ix}]}";

      clear_part_task_fields;
      clear_option_color;
      get_participant_ids;
      get_image_files;

      clear;
      printf "${BBlue}EXP: ${BGreen}${exp_id}  ${BBlue}FOR: ${BGreen}${part_id}  ${BBlue}ID: ${BGreen}${cur_id}  ${BBlue}TASK: ${BGreen}${task_id}  ${BBlue}DT: ${BGreen}${task_date}  ${BBlue}FACE: ${BPurple}${face_name}  ${BBlue}SCREEN: ${BPurple}${scrn_name}${NC}\n";

      if ( ${cur_exp} ); then
        record_check "1";
        record_check "2";
      else
        if [[ "${task_date}" == "" ]]; then
          record_check "3";
        else
          if ! test -f "${face_file}" ; then
            record_check "f";
          else
            if ! test -f "${scrn_file}" ; then
              record_check "s";
            fi
          fi
        fi
      fi

      if [[ "${face_name}" != "" ]]; then
        ~/.iterm2/imgcat "${face_file}";
      fi
      if [[ "${scrn_name}" != "" ]]; then
        ~/.iterm2/imgcat "${scrn_file}";
      fi
      input_checks;

    done
  done
  log_data "WRITE";
  printf "\n****** DONE WITH EXP: ${exp_id} ******\n\n";
  read -n 1 -r -s -p "<Press any key for next Experiment>";
done

printf "\n************ DONE WITH ALL EXPERIMENTS ************\n\n\n";


exit;
