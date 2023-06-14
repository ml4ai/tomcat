#!/bin/bash

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

# Home
# in_mission_file="/mnt/c/Users/rcham/Documents/ToMCAT/Lab/view_exp_face_screen/mission.csv";
# in_participant_ids_file="/mnt/c/Users/rcham/Documents/ToMCAT/Lab/view_exp_face_screen/participant_ids.csv";


# out_file="view_exp_face_screen.csv";

# All Experiments:
# experiments="exp_2022_09_30_10 exp_2022_10_04_09 exp_2022_10_07_15 exp_2022_10_14_10 exp_2022_10_18_10 exp_2022_10_21_15 exp_2022_10_24_12 exp_2022_10_27_10 exp_2022_10_28_10 exp_2022_10_31_10 exp_2022_11_01_10 exp_2022_11_04_10 exp_2022_11_07_10 exp_2022_11_08_11 exp_2022_11_10_10 exp_2022_11_14_12 exp_2022_11_15_13 exp_2022_11_17_15 exp_2022_11_18_10 exp_2022_11_22_10 exp_2022_12_02_15 exp_2022_12_05_12 exp_2023_01_30_13 exp_2023_01_31_14 exp_2023_02_03_10 exp_2023_02_06_13 exp_2023_02_07_14 exp_2023_02_10_10 exp_2023_02_16_14 exp_2023_02_20_01 exp_2023_02_20_13 exp_2023_02_21_14 exp_2023_04_17_13 exp_2023_04_18_14 exp_2023_04_20_14 exp_2023_04_21_10 exp_2023_04_24_13 exp_2023_04_27_14 exp_2023_04_28_10
# exp_2023_05_01_13 exp_2023_05_02_14 exp_2023_05_03_10";
# out_file="view_exp_face_screen_all.csv";

# 2022 Experiments:
experiments="exp_2022_09_30_10 exp_2022_10_04_09 exp_2022_10_07_15 exp_2022_10_14_10 exp_2022_10_18_10 exp_2022_10_21_15 exp_2022_10_24_12 exp_2022_10_27_10 exp_2022_10_28_10 exp_2022_10_31_10 exp_2022_11_01_10 exp_2022_11_04_10 exp_2022_11_07_10 exp_2022_11_08_11 exp_2022_11_10_10 exp_2022_11_14_12 exp_2022_11_15_13 exp_2022_11_17_15 exp_2022_11_18_10 exp_2022_11_22_10 exp_2022_12_02_15 exp_2022_12_05_12";
out_file="view_exp_face_screen_2022.csv";

# 2023 Experiments up to the new pipeline exp_2023_04_17_13:
# experiments="exp_2023_01_30_13 exp_2023_01_31_14 exp_2023_02_03_10 exp_2023_02_06_13 exp_2023_02_07_14 exp_2023_02_10_10 exp_2023_02_16_14 exp_2023_02_20_01 exp_2023_02_20_13 exp_2023_02_21_14";
# out_file="view_exp_face_screen_2023.csv";

# New Pipeline Experiments:
# experiments="exp_2023_04_17_13 exp_2023_04_18_14 exp_2023_04_20_14 exp_2023_04_21_10 exp_2023_04_24_13 exp_2023_04_27_14 exp_2023_04_28_10 exp_2023_05_01_13 exp_2023_05_02_14 exp_2023_05_03_10";
# out_file="view_exp_face_screen_new_pipeline.csv";

# New Pipeline Experiments just exp_2023_04_21_10:
# experiments="exp_2023_04_21_10";
# out_file="view_exp_face_screen_new_pipeline_exp_2023_04_21_10.csv";

# For TESTING:
# experiments="exp_2023_02_21_14";

# exp_2023_01_30_13 - face: output066105.png    screen: output061975.png
# exp_2023_01_31_14 - face: output043524.png    screen: output039402.png
# exp_2023_02_03_10 - face: 2023-02-03T20_08_54.939625867Z.png and output069731.png    screen: 2023-02-03T20_08_01.521426496Z.png
# exp_2023_02_06_13 - face: 2023-02-06T22_54_49.226953209Z.png    screen: 2023-02-06T22_54_49.270954999Z.png
# exp_2023_02_07_14 - face: 2023-02-07T23_58_09.472672446Z.png    screen: 2023-02-07T23_58_09.096657032Z.png

# exp_2023_02_21_14 - face:  2023-02-21T23_17_24.715186050Z.png and output059988.png    screen: 2023-02-21T23_37_58.078057554Z.png

# exp_2023_04_17_13/lion/screenshots/block_1 - face: 020704_2023-04-17_21-59-22.439.PM~157.png    screen: 009729_2023-04-17_21-59-25.713.PM~268.png
# exp_2023_04_17_13/lion/screenshots/block_2 - face: 025188_2023-04-17_22-50-41.135.PM~110.png    screen: 010861_2023-04-17_22-50-42.577.PM~208.png

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

fields="experiment_id lion_eeg_data_rest_state lion_eeg_data_finger_tapping lion_eeg_data_affective_task_individual lion_eeg_data_affective_task_team lion_eeg_data_ping_pong_competetive_0 lion_eeg_data_ping_pong_competetive_1 lion_eeg_data_ping_pong_cooperative_0 lion_eeg_data_hands_on_training lion_eeg_data_saturn_a lion_eeg_data_saturn_b tiger_eeg_data_rest_state tiger_eeg_data_finger_tapping tiger_eeg_data_affective_task_individual tiger_eeg_data_affective_task_team tiger_eeg_data_ping_pong_competetive_0 tiger_eeg_data_ping_pong_competetive_1 tiger_eeg_data_ping_pong_cooperative_0 tiger_eeg_data_hands_on_training tiger_eeg_data_saturn_a tiger_eeg_data_saturn_b leopard_eeg_data_rest_state leopard_eeg_data_finger_tapping leopard_eeg_data_affective_task_individual leopard_eeg_data_affective_task_team leopard_eeg_data_ping_pong_competetive_0 leopard_eeg_data_ping_pong_competetive_1 leopard_eeg_data_ping_pong_cooperative_0 leopard_eeg_data_hands_on_training leopard_eeg_data_saturn_a leopard_eeg_data_saturn_b lion_nirs_data_rest_state lion_nirs_data_finger_tapping lion_nirs_data_affective_task_individual lion_nirs_data_affective_task_team lion_nirs_data_ping_pong_competetive_0 lion_nirs_data_ping_pong_competetive_1 lion_nirs_data_ping_pong_cooperative_0 lion_nirs_data_hands_on_training lion_nirs_data_saturn_a lion_nirs_data_saturn_b tiger_nirs_data_rest_state tiger_nirs_data_finger_tapping tiger_nirs_data_affective_task_individual tiger_nirs_data_affective_task_team tiger_nirs_data_ping_pong_competetive_0 tiger_nirs_data_ping_pong_competetive_1 tiger_nirs_data_ping_pong_cooperative_0 tiger_nirs_data_hands_on_training tiger_nirs_data_saturn_a tiger_nirs_data_saturn_b leopard_nirs_data_rest_state leopard_nirs_data_finger_tapping leopard_nirs_data_affective_task_individual leopard_nirs_data_affective_task_team leopard_nirs_data_ping_pong_competetive_0 leopard_nirs_data_ping_pong_competetive_1 leopard_nirs_data_ping_pong_cooperative_0 leopard_nirs_data_hands_on_training leopard_nirs_data_saturn_a leopard_nirs_data_saturn_b lion_pupil_data_rest_state lion_pupil_data_finger_tapping lion_pupil_data_affective_task_individual lion_pupil_data_affective_task_team lion_pupil_data_ping_pong_competetive_0 lion_pupil_data_ping_pong_competetive_1 lion_pupil_data_ping_pong_cooperative_0 lion_pupil_data_hands_on_training lion_pupil_data_saturn_a lion_pupil_data_saturn_b tiger_pupil_data_rest_state tiger_pupil_data_finger_tapping tiger_pupil_data_affective_task_individual tiger_pupil_data_affective_task_team tiger_pupil_data_ping_pong_competetive_0 tiger_pupil_data_ping_pong_competetive_1 tiger_pupil_data_ping_pong_cooperative_0 tiger_pupil_data_hands_on_training tiger_pupil_data_saturn_a tiger_pupil_data_saturn_b leopard_pupil_data_rest_state leopard_pupil_data_finger_tapping leopard_pupil_data_affective_task_individual leopard_pupil_data_affective_task_team leopard_pupil_data_ping_pong_competetive_0 leopard_pupil_data_ping_pong_competetive_1 leopard_pupil_data_ping_pong_cooperative_0 leopard_pupil_data_hands_on_training leopard_pupil_data_saturn_a leopard_pupil_data_saturn_b";

oc_off="${BWhite}${On_Black}";
oc_red="${Black}${On_Red}";
oc_yellow="${Black}${On_Yellow}";
oc_green="${Black}${On_Green}";
oc0="${NC} ${Blue}|${NC} ";
ocn="${BBlue}";

eval "exp_arr=(${experiments})";
eval "part_arr=(${participants})";
eval "cap_types_arr=(${cap_types})";
eval "gls_types_arr=(${gls_types})";
eval "task_arr=(${tasks})";
eval "log_tasks_arr=(${log_tasks})";
eval "baseline_tasks_arr=(${baseline_tasks})";
eval "field_arr=(${fields})";



log_data() {
  # For TESTING:
  # printf "\nlog_data - field (arg 1): ${1}      reason (arg 2): ${2}\n";
  # sleep 2;

  if [[ "${1}" == "RESET" ]]; then
    if test -f "${out_file}" ; then
      rm "${out_file}";
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
    return 0;
  fi

  if [[ "${1}" == "WRITE" ]]; then
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
    # line_out="|${Exp_ID}|,|a_test_data_for_testing|\n";
    printf "${line_out//|/\"}" >> "${out_file}";
    return 0;
  fi

  if [[ "${1}" == "CLEAR" ]]; then
    for (( i=0; i<"${#field_arr[@]}"; i++ )); do
      eval "${field_arr[${i}]}"='';
    done
    return 0;
  fi

  for (( i=0; i<"${#field_arr[@]}"; i++ )); do
    if [[ "${1}" == "${field_arr[${i}]}" ]]; then
      eval "${field_arr[${i}]}"='${2}';
      break;
    fi
  done

}


clear_option_color() {
  oc1="${oc_off}";
  oc2="${oc_off}";
  oc3="${oc_off}";
  ocf="${oc_off}";
  ocs="${oc_off}";
  ocx="${oc_off}";
  ocy="${oc_off}";
  occ="${oc_off}";
  ocg="${oc_off}";
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
  # For TESTING:
  # if [[ "${2}" == "glasses" ]]; then
  #   eval "data_types_arr=(${gls_types})";
  #   printf "\nglasses - part_id: ${part_id}   task_id: ${task_id}   reason: ${1}\n";
  # else
  #   eval "data_types_arr=(${cap_types})";
  #   printf "\ncap - part_id: ${part_id}   task_id: ${task_id}   reason: ${1}\n";
  # fi

  case ${task_id} in
    Start)
      for data_type_id in "${data_types_arr[@]}"; do
        for baseline_task_id in "${baseline_tasks_arr[@]}"; do
          log_data "${part_id}_${data_type_id}_data_${baseline_task_id}" "${1}"
        done
      done
    ;;
    Hands-on) 
      for data_type_id in "${data_types_arr[@]}"; do
        log_data "${part_id}_${data_type_id}_data_hands_on_training" "${1}"
      done
    ;;
    Saturn_A) 
      for data_type_id in "${data_types_arr[@]}"; do
        log_data "${part_id}_${data_type_id}_data_saturn_a" "${1}"
      done
    ;;
    Saturn_B) 
      for data_type_id in "${data_types_arr[@]}"; do
        log_data "${part_id}_${data_type_id}_data_saturn_b" "${1}"
      done
    ;;
  esac
}


record_check() {
  # tasks="Start Hands-on Saturn_A Saturn_B";
  # printf "record_check - arg 1: ${1}\n";
  # printf "1=Exp-No_Cap|2=Exp-No_Gls|6=Part-No_Face|7=Part-No_Scrn|x=Part-No_Cap|y=Part-No_Gls|c=Part-Cap_On|g=Part-Gls_On|n=Next-Pic";

  case $1 in
    1) # Experimenter, No Cap On.
      toggle_reason "experimenter_no_cap_on" "all" "${1}" "${oc_red}";
      log_reason "${reason}" "cap";
    ;;
  
    2) # Experimenter, No Glasses On.
      toggle_reason "experimenter_no_glasses_on" "all" "${1}" "${oc_red}";
      log_reason "${reason}" "glasses";
    ;;

    3) # No Minecraft Mission Start Time Found, per Mission.
      if [[ "${task_id}" != "Start" ]]; then
        toggle_reason "misson_start_time_not_found" "${task_id}" "${1}" "${oc_red}";
        log_reason "${reason}" "cap";
        log_reason "${reason}" "glasses";
      else
        printf "\r${BRed}*************************************************************** Not on Minecraft Mission Task *******************************************************************${NC}";
        sleep 3;
      fi
    ;;

    f) # Participant, No Face Image, per Task.
      toggle_reason "no_face_image" "${task_id}" "${1}" "${oc_red}";
      log_reason "${reason}" "cap";
      log_reason "${reason}" "glasses";
    ;;

    s) # Participant, No Screen Image, per Task.
      toggle_reason "no_screen_image" "${task_id}" "${1}" "${oc_red}";
      log_reason "${reason}" "cap";
      log_reason "${reason}" "glasses";
    ;;

    x) # Participant, No Cap On, per task
      toggle_reason "participant_no_cap_on" "${task_id}" "${1}" "${oc_red}";
      log_reason "${reason}" "cap"
    ;;
  
    y) # Participant, No Glasses On, per task
      toggle_reason "participant_no_glasses_on" "${task_id}" "${1}" "${oc_red}";
      log_reason "${reason}" "glasses"
    ;;

    c) # Participant, per task -  Cap Is On
      toggle_reason "ok" "${task_id}" "${1}" "${oc_green}";
      log_reason "${reason}" "cap"
    ;;
  
    g) # Participant, per task -  Glasses Are On
      toggle_reason "ok" "${task_id}" "${1}" "${oc_green}";
      log_reason "${reason}" "glasses"
    ;;

    o) # Participant, per task -  Cap Is On and Glasses Are On
      toggle_reason "ok" "${task_id}" "c" "${oc_green}";
      log_reason "${reason}" "cap"
      toggle_reason "ok" "${task_id}" "g" "${oc_green}";
      log_reason "${reason}" "glasses"
    ;;

    *) printf "\r${BRed}********************************************************************* Not a valid option ************************************************************************${NC}";
      sleep 3;
    ;;
  esac
}


input_checks() {
  ch_rept=true;
  while ( ${ch_rept} )
  do
    # option_color
    printf "${oc1}1=Exp-No_Cap${oc0}${oc2}2=Exp-No_Gls${oc0}${oc3}3=No_Mission_Start${oc0}${ocf}f=Part-No_Face${oc0}${ocs}s=Part-No_Scrn${oc0}${ocx}x=Part-No_Cap${oc0}${ocy}y=Part-No_Gls${oc0}${occ}c=Part-Cap_On${oc0}${ocg}g=Part-Gls_On${oc0}${ocn}n=Next-Pics>${NC}";
    read -s -N 1 ch_input
    if [[ "${ch_input}" == "n" ]]; then ch_rept=false; printf "\n"; return 1; fi;
    record_check "${ch_input}";
    if ( ${ch_rept} ); then  printf "\r                                                                                                                                                                 \r"; fi;
  done
}


get_participant_ids() {
  ids_line="";
  lion_id="";
  tiger_id="";
  leopard_id="";
  cur_id="";
  cur_exp=false;
  ids_line=$(grep "${exp_id}" "${in_participant_ids_file}");
  if [[ "${ids_line}" != "" ]]; then
    IFS=',' read -r -a ids_line_arr <<< "${ids_line}";
    lion_id="${ids_line_arr[3]}";
    tiger_id="${ids_line_arr[4]}";
    leopard_id="${ids_line_arr[5]}";
    id_name="${part_id}_id";
    cur_id="${!id_name}";
    if [[ "${cur_id: 0: 5}" == "99999" ]]; then cur_exp=true; else cur_exp=false; fi
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
    if [[ "${exp_id: 4: 10}" < "2023_02_03" ]]; then
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

    if [[ "${exp_id: 4: 10}" < "2023_02_03" ]]; then
      # Old Image Name Format: output000500.png
      face_dir="face_images/";
      scrn_dir="screenshots/";
      face_fdir="${in_dir}${part_id}/${face_dir}";
      scrn_fdir="${in_dir}${part_id}/${scrn_dir}";
 
      face_name="";
      file_line="";
      # unset line_arr;
      # -rw-r--r-- 1 root root  671606 2022-09-30 13:09:13.698767910 -0700 output063601.png
      file_line=$(ls --full-time "${face_fdir}" | grep "${task_date}" | head -1); ****************************** Maybe wrong task date!
      # file_line="${file_line//" "/","}";
      # IFS=',' read -r -a line_arr <<< "${file_line}";
      # face_name="${line_arr[9]}"
      if [[ "${file_line}" != "" ]]; then face_name="${file_line: -16: 16}"; fi

      scrn_name="";
      file_line="";
      # unset line_arr;
      # -rw-r--r-- 1 root root  934543 2022-09-30 13:08:57.450100667 -0700 output056565.png
      file_line=$(ls --full-time "${scrn_fdir}" | grep "${task_date}" | head -1); ****************************** Maybe wrong task date!
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





# For TESTING:
# exp_id="exp_1961_09_30_09";

# log_data "RESET"
# log_data "CLEAR"
# log_data "HEADER"

# for part_id in "${part_arr[@]}"; do
#   for task_id in "${task_arr[@]}"; do
#     # clear;
#     printf "\n\n\nDisplaying - EXP: ${exp_id}  FOR: ${part_id}  TASK: ${task_id}\n";
#     input_checks;
#   done
# done

# log_data "WRITE"
# exit;




# eval "exp_arr=(${experiments})";
# eval "part_arr=(${participants})";
# eval "task_arr=(${tasks})";
# eval "scrn_arr=(${scrn_images})";

log_data "RESET";
log_data "HEADER";

for exp_id in "${exp_arr[@]}"; do
  
  in_dir="${in_root_dir}${exp_id}/";

  log_data "CLEAR";

  for part_id in "${part_arr[@]}"; do
    for task_id in "${task_arr[@]}"; do

        clear_option_color;
        get_participant_ids;
        get_image_files;

        clear;
        printf "*** EXP: ${exp_id}  FOR: ${part_id}  ID: ${cur_id}  TASK: ${task_id}  DT: ${task_date}  FACE: ${face_name}  SCREEN: ${scrn_name}\n";

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
