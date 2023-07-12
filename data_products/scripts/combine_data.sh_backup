#!/bin/bash

my_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"


# Dev Settings:
show_eeg_data_checks=false;
show_nir_data_checks=false;


# On gauss:
# in_root_dir="/space/calebshibu/Neurips_new/";
# out_root_dir="/space/rchamplin/Neurips/";

# At Home
in_root_dir="/mnt/c/Users/rcham/Documents/ToMCAT/Lab/combine_data/space/calebshibu/Neurips/";
out_root_dir="/mnt/c/Users/rcham/Documents/ToMCAT/Lab/combine_data/space/rchamplin/Neurips/";

log_file="combine_data_eeg_baseline.log";

experiments="exp_2023_02_21_14 exp_2022_09_30_10 exp_2022_10_04_09 exp_2022_10_07_15 exp_2022_10_14_10 exp_2022_10_18_10 exp_2022_10_21_15 exp_2022_10_24_12 exp_2022_10_27_10 exp_2022_10_28_10 exp_2022_10_31_10 exp_2022_11_01_10 exp_2022_11_04_10 exp_2022_11_07_10 exp_2022_11_08_11 exp_2022_11_10_10 exp_2022_11_14_12 exp_2022_11_15_13 exp_2022_11_17_15 exp_2022_11_18_10 exp_2022_11_22_10 exp_2022_12_02_15 exp_2022_12_05_12 exp_2023_01_30_13 exp_2023_01_31_14 exp_2023_02_03_10 exp_2023_02_06_13 exp_2023_02_07_14 exp_2023_02_10_10 exp_2023_02_16_14 exp_2023_02_20_01";

participants="lion tiger leopard";

types="EEG NIRS";

tasks="rest_state finger_tapping affective_task_individual affective_task_team ping_pong_competetive_0 ping_pong_competetive_1 ping_pong_cooperative_0 hands_on_training saturn_a saturn_b";

in_file_suf="_filtered.csv"; # ${type_id}${in_file_suf}  Example: EEG_filtered.csv
out_file_suf=".csv"; # ${part_id}_${type_id,,}_${task_id}${out_file_suf}  Example: lion_eeg_rest_state.csv


delete_no_data_file=true;

# Notes:

# event_type="rest_state";
# event_type="finger_tapping";
# event_type="affective_task_individual";
# event_type="affective_task_team";
# event_type="ping_pong_competetive_0";
# event_type="ping_pong_competetive_1";
# event_type="ping_pong_cooperative_0";
# event_type="hands_on_training";
# event_type="saturn_a";
# event_type="saturn_b";

# In rchamplin@gauss:/space/calebshibu/Neurips_new/
# *** exp_2022_09_09_12  exp_2022_10_18_10  exp_2022_11_01_10  exp_2022_11_15_13  exp_2023_01_30_13  exp_2023_02_16_14
# *** exp_2022_09_29_15  exp_2022_10_21_15  exp_2022_11_04_10  exp_2022_11_17_15  exp_2023_01_31_14  exp_2023_02_20_01
# exp_2022_09_30_10  exp_2022_10_24_12  exp_2022_11_07_10  exp_2022_11_18_10  exp_2023_02_03_10  *** exp_2023_02_20_13 (dont have in my records)
# exp_2022_10_04_09  exp_2022_10_27_10  exp_2022_11_08_11  exp_2022_11_22_10  exp_2023_02_06_13  exp_2023_02_21_14
# exp_2022_10_07_15  exp_2022_10_28_10  exp_2022_11_10_10  exp_2022_12_02_15  exp_2023_02_07_14
# exp_2022_10_14_10  exp_2022_10_31_10  exp_2022_11_14_12  exp_2022_12_05_12  exp_2023_02_10_10





log_line(){
  lf="${out_dir}${log_file}";
  if [[ "${1}" == "RESET" ]]; then
    if test -f "${lf}" ; then
      rm "${lf}";
    fi
  else
    if [ ! -f "${lf}" ]; then
      touch "${lf}";
    fi
    dt=$(date +%Y-%m-%d_%H:%M:%S);
    printf "${dt} - ${1}" >> "${lf}";
  fi
}


create_eeg_files(){
  # Passed in argument examples:
  # $1 = part_name "Lion";
  # $2 = task_name "ping_pong_competetive_0";
  # $3 = in_file "${in_dir}lion/EEG_filtered.csv";
  # $4 = out_file "${out_dir}lion_eeg_affective_task_individual.csv"";

  # EEG Header:
  # "        AFF1h   F7      FC5     C3      T7      TP9     Pz      P3      P7      O1      O2      P8      P4      TP10    Cz      C4      T8      FC6     FCz     F8      AFF2h   AUX_GSR AUX_EKG human_readable_time    unix_time       event_type"


  part_name="${1}";
  task_name="${2}";
  in_file="${3}";
  out_file="${4}";

  # If the output dir does not exist, create it. 
  if [ ! -d "${out_dir}" ]; then
    mkdir -p ${out_dir}
  fi

  date +%Y-%m-%d_%H:%M:%S
  printf "*** STARTING: EXP: ${exp_id}   EEG ${part_name} - Task: ${task_name}\n              In File: ${in_file}\n              Out File: ${out_file}\n";
  log_line "*** STARTING: EXP: ${exp_id}   EEG ${part_name} - Task: ${task_name}\n              In File: ${in_file}\n              Out File: ${out_file}\n";

  # If the input file does not exist, print error and jump out of sub routine. 
  if ! test -f "${in_file}" ; then
      printf "\n*** ERROR: Input File Does Not Exist: ${in_file}  *** Routine Aborted ***\n\n\n";
      log_line "*** ERROR: Input File Does Not Exist: ${in_file}  *** Routine Aborted ***\n\n\n";
      return 0;
  fi

  unset line_array;
  group_found=false;
  found_count=0;

  if test -f "${out_file}" ; then
    rm "${out_file}";
  fi
# out_line="line,AFF1h,F7,FC5,C3,T7,TP9,Pz,P3,P7,O1,O2,P8,P4,TP10,Cz,C4,T8,FC6,FCz,F8,AFF2h,AUX_GSR,AUX_EKG,human_readable_time,unix_time,event_type\n";
  out_line="${part_name}_eeg_line,${part_name}_eeg_AFF1h,${part_name}_eeg_F7,${part_name}_eeg_FC5,${part_name}_eeg_C3,${part_name}_eeg_T7,${part_name}_eeg_TP9,${part_name}_eeg_Pz,${part_name}_eeg_P3,${part_name}_eeg_P7,${part_name}_eeg_O1,${part_name}_eeg_O2,${part_name}_eeg_P8,${part_name}_eeg_P4,${part_name}_eeg_TP10,${part_name}_eeg_Cz,${part_name}_eeg_C4,${part_name}_eeg_T8,${part_name}_eeg_FC6,${part_name}_eeg_FCz,${part_name}_eeg_F8,${part_name}_eeg_AFF2h,${part_name}_eeg_AUX_GSR,${part_name}_eeg_AUX_EKG,${part_name}_eeg_human_readable_time,${part_name}_eeg_unix_time,${part_name}_eeg_event_type\n";
  printf "${out_line}" >> "${out_file}";

  printf "Records Found: ${found_count}";

  while IFS= read -r line
  do
    IFS=$'|' line_array=(${line//$'\t'/|})
    if [[ "${line_array[26]}" == "${task_name}" ]]; then
      group_found=true;
      ((found_count=found_count+1));
      time_s="${line_array[24]}";
      time_ns=${time_s//" "/"_"};
      out_line_q="${line_array[0]},${line_array[1]},${line_array[2]},${line_array[3]},${line_array[4]},${line_array[5]},${line_array[6]},${line_array[7]},${line_array[8]},${line_array[9]},${line_array[10]},${line_array[11]},${line_array[12]},${line_array[13]},${line_array[14]},${line_array[15]},${line_array[16]},${line_array[17]},${line_array[18]},${line_array[19]},${line_array[20]},${line_array[21]},${line_array[22]},${line_array[23]},'${time_ns}','${line_array[25]}','${line_array[26]}'\n";
      out_line=${out_line_q//\'/\"};
      printf "${out_line}" >> "${out_file}";

      printf "\rRecords Found: ${found_count}";
    else
      if ( ${group_found} ); then
        printf "\nEEG ${part_name} - Done with Task: ${task_name}   Found Line Count: ${found_count}    *** Searching for Records Complete ***\n";
        log_line "EEG ${part_name} - Done with Task: ${task_name}   Found Line Count: ${found_count}    *** Searching for Records Complete ***\n";
        break;
      fi
    fi
  done < "${in_file}"

  unset line_array;

  # If No records found in the Input File for the Task, print/log and exit routine.
  if [ ${found_count} -eq 0 ]; then

    if ( ${delete_no_data_file} ); then
      if test -f "${out_file}" ; then
        rm "${out_file}";
        printf "\nOutput File Deleted Because No Data Lines Found! (it was just a header row) No Records Found For EEG ${part_name} - Task: ${task_name}\n      Output File Deleted: ${out_file}\n";
        log_line "Output File Deleted Because No Data Lines Found! (it was just a header row) No Records Found For EEG ${part_name} - Task: ${task_name}\n      Output File Deleted: ${out_file}\n";
      fi
    fi

    printf "\n*** ERROR: No Records Found For EEG ${part_name} - Task: ${task_name}  *** Routine Aborted ***\n              In File: ${in_file}\n\n\n";
    log_line "*** ERROR: No Records Found For EEG ${part_name} - Task: ${task_name}  *** Routine Aborted ***\n              In File: ${in_file}\n\n\n";
    return 0;
  fi

  printf "\n*** COMPLETED: EXP: ${exp_id}   EEG ${part_name} - Task: ${task_name} ***\n";
  log_line "*** COMPLETED: EXP: ${exp_id}   EEG ${part_name} - Task: ${task_name} ***\n\n\n";
  date +%Y-%m-%d_%H:%M:%S
  printf "\n\n\n";

}




create_nirs_files(){
  # Passed in argument examples:
  # $1 = part_name "Lion";
  # $2 = task_name "ping_pong_competetive_0";
  # $3 = in_file "${in_dir}lion/NIRS_filtered.csv";
  # $4 = out_file "${out_dir}lion_nirs_affective_task_individual.csv"";

  # NIRS Header:
  # "	S1-D1_HbO	S1-D2_HbO	S2-D1_HbO	S2-D3_HbO	S3-D1_HbO	S3-D3_HbO	S3-D4_HbO	S4-D2_HbO	S4-D4_HbO	S4-D5_HbO	S5-D3_HbO	S5-D4_HbO	S5-D6_HbO	S6-D4_HbO	S6-D6_HbO	S6-D7_HbO	S7-D5_HbO	S7-D7_HbO	S8-D6_HbO	S8-D7_HbO	S1-D1_HbR	S1-D2_HbR	S2-D1_HbR	S2-D3_HbR	S3-D1_HbR	S3-D3_HbR	S3-D4_HbR	S4-D2_HbR	S4-D4_HbR	S4-D5_HbR	S5-D3_HbR	S5-D4_HbR	S5-D6_HbR	S6-D4_HbR	S6-D6_HbR	S6-D7_HbR	S7-D5_HbR	S7-D7_HbR	S8-D6_HbR	S8-D7_HbR	human_readable_time	unix_time	event_type"


  part_name="${1}";
  task_name="${2}";
  in_file="${3}";
  out_file="${4}";

  # If the output dir does not exist, create it. 
  if [ ! -d "${out_dir}" ]; then
    mkdir -p ${out_dir}
  fi

  date +%Y-%m-%d_%H:%M:%S
  printf "*** STARTING: EXP: ${exp_id}   NIRS ${part_name} - Task: ${task_name}\n              In File: ${in_file}\n              Out File: ${out_file}\n";
  log_line "*** STARTING: EXP: ${exp_id}   NIRS ${part_name} - Task: ${task_name}\n              In File: ${in_file}\n              Out File: ${out_file}\n";

  # If the input file does not exist, print error and jump out of sub routine. 
  if ! test -f "${in_file}" ; then
      printf "\n*** ERROR: Input File Does Not Exist: ${in_file}  *** Routine Aborted ***\n\n\n";
      log_line "*** ERROR: Input File Does Not Exist: ${in_file}  *** Routine Aborted ***\n\n\n";
      return 0;
  fi

  unset line_array;
  group_found=false;
  found_count=0;

  if test -f "${out_file}" ; then
    rm "${out_file}";
  fi
# out_line="line,S1_D1_HbO,S1_D2_HbO,S2_D1_HbO,S2_D3_HbO,S3_D1_HbO,S3_D3_HbO,S3_D4_HbO,S4_D2_HbO,S4_D4_HbO,S4_D5_HbO,S5_D3_HbO,S5_D4_HbO,S5_D6_HbO,S6_D4_HbO,S6_D6_HbO,S6_D7_HbO,S7_D5_HbO,S7_D7_HbO,S8_D6_HbO,S8_D7_HbO,S1_D1_HbR,S1_D2_HbR,S2_D1_HbR,S2_D3_HbR,S3_D1_HbR,S3_D3_HbR,S3_D4_HbR,S4_D2_HbR,S4_D4_HbR,S4_D5_HbR,S5_D3_HbR,S5_D4_HbR,S5_D6_HbR,S6_D4_HbR,S6_D6_HbR,S6_D7_HbR,S7_D5_HbR,S7_D7_HbR,S8_D6_HbR,S8_D7_HbR,human_readable_time,unix_time,event_type\n";
  out_line="${part_name}_nirs_line,${part_name}_nirs_S1_D1_HbO,${part_name}_nirs_S1_D2_HbO,${part_name}_nirs_S2_D1_HbO,${part_name}_nirs_S2_D3_HbO,${part_name}_nirs_S3_D1_HbO,${part_name}_nirs_S3_D3_HbO,${part_name}_nirs_S3_D4_HbO,${part_name}_nirs_S4_D2_HbO,${part_name}_nirs_S4_D4_HbO,${part_name}_nirs_S4_D5_HbO,${part_name}_nirs_S5_D3_HbO,${part_name}_nirs_S5_D4_HbO,${part_name}_nirs_S5_D6_HbO,${part_name}_nirs_S6_D4_HbO,${part_name}_nirs_S6_D6_HbO,${part_name}_nirs_S6_D7_HbO,${part_name}_nirs_S7_D5_HbO,${part_name}_nirs_S7_D7_HbO,${part_name}_nirs_S8_D6_HbO,${part_name}_nirs_S8_D7_HbO,${part_name}_nirs_S1_D1_HbR,${part_name}_nirs_S1_D2_HbR,${part_name}_nirs_S2_D1_HbR,${part_name}_nirs_S2_D3_HbR,${part_name}_nirs_S3_D1_HbR,${part_name}_nirs_S3_D3_HbR,${part_name}_nirs_S3_D4_HbR,${part_name}_nirs_S4_D2_HbR,${part_name}_nirs_S4_D4_HbR,${part_name}_nirs_S4_D5_HbR,${part_name}_nirs_S5_D3_HbR,${part_name}_nirs_S5_D4_HbR,${part_name}_nirs_S5_D6_HbR,${part_name}_nirs_S6_D4_HbR,${part_name}_nirs_S6_D6_HbR,${part_name}_nirs_S6_D7_HbR,${part_name}_nirs_S7_D5_HbR,${part_name}_nirs_S7_D7_HbR,${part_name}_nirs_S8_D6_HbR,${part_name}_nirs_S8_D7_HbR,${part_name}_nirs_human_readable_time,${part_name}_nirs_unix_time,${part_name}_nirs_event_type\n";
  printf "${out_line}" >> "${out_file}";

  printf "Records Found: ${found_count}";

  while IFS= read -r line
  do
    IFS=$'|' line_array=(${line//$'\t'/|})
    if [[ "${line_array[43]}" == "${task_name}" ]]; then
      group_found=true;
      ((found_count=found_count+1));
      time_s="${line_array[41]}";
      time_ns=${time_s//" "/"_"};
      out_line_q="${line_array[0]},${line_array[1]},${line_array[2]},${line_array[3]},${line_array[4]},${line_array[5]},${line_array[6]},${line_array[7]},${line_array[8]},${line_array[9]},${line_array[10]},${line_array[11]},${line_array[12]},${line_array[13]},${line_array[14]},${line_array[15]},${line_array[16]},${line_array[17]},${line_array[18]},${line_array[19]},${line_array[20]},${line_array[21]},${line_array[22]},${line_array[23]},${line_array[24]},${line_array[25]},${line_array[26]},${line_array[27]},${line_array[28]},${line_array[29]},${line_array[30]},${line_array[31]},${line_array[32]},${line_array[33]},${line_array[34]},${line_array[35]},${line_array[36]},${line_array[37]},${line_array[38]},${line_array[39]},${line_array[40]},'${time_ns}','${line_array[42]}','${line_array[43]}'\n";
      out_line=${out_line_q//\'/\"};
      printf "${out_line}" >> "${out_file}";

      printf "\rRecords Found: ${found_count}";
    else
      if ( ${group_found} ); then
        printf "\NIRS ${part_name} - Done with Task: ${task_name}   Found Line Count: ${found_count}    *** Searching for Records Complete ***\n";
        log_line "NIRS ${part_name} - Done with Task: ${task_name}   Found Line Count: ${found_count}    *** Searching for Records Complete ***\n";
        break;
      fi
    fi
  done < "${in_file}"

  unset line_array;

  # If No records found in the Input File for the Task, print/log and exit routine.
  if [ ${found_count} -eq 0 ]; then
  
    if ( ${delete_no_data_file} ); then
      if test -f "${out_file}" ; then
        rm "${out_file}";
        printf "\nOutput File Deleted Because No Data Lines Found! (it was just a header row) No Records Found For NIRS ${part_name} - Task: ${task_name}\n      Output File Deleted: ${out_file}\n";
        log_line "Output File Deleted Because No Data Lines Found! (it was just a header row) No Records Found For NIRS ${part_name} - Task: ${task_name}\n      Output File Deleted: ${out_file}\n";
      fi
    fi

    printf "\n*** ERROR: No Records Found For NIRS ${part_name} - Task: ${task_name}  *** Routine Aborted ***\n              In File: ${in_file}\n\n\n";
    log_line "*** ERROR: No Records Found For NIRS ${part_name} - Task: ${task_name}  *** Routine Aborted ***\n              In File: ${in_file}\n\n\n";
    return 0;
  fi

  printf "\n*** COMPLETED: EXP: ${exp_id}   NIRS ${part_name} - Task: ${task_name} ***\n";
  log_line "*** COMPLETED: EXP: ${exp_id}   NIRS ${part_name} - Task: ${task_name} ***\n\n\n";
  date +%Y-%m-%d_%H:%M:%S
  printf "\n\n\n";
}



# Passed argument examples:
# $1 = part_name "Lion";
# $2 = task_name "ping_pong_competetive_0";
# $3 = in_file "${in_dir}lion/EEG_filtered.csv";
# $4 = out_file "${out_dir}lion_eeg_affective_task_individual.csv"";

# participants="lion tiger leopard";
# types="EEG NIRS";
# tasks="rest_state finger_tapping affective_task_individual affective_task_team ping_pong_competetive_0 ping_pong_competetive_1 ping_pong_cooperative_0 hands_on_training saturn_a saturn_b";


eval "exp_arr=(${experiments})";
eval "part_arr=(${participants})";
eval "type_arr=(${types})";
eval "task_arr=(${tasks})";

for exp_id in "${exp_arr[@]}"; do
  
  in_dir="${in_root_dir}${exp_id}/";
  out_dir="${out_root_dir}${exp_id}/";

  log_line "RESET";

  for part_id in "${part_arr[@]}"; do
    for type_id in "${type_arr[@]}"; do
      for task_id in "${task_arr[@]}"; do
        if [[ "${type_id}" == "EEG" ]]; then
          create_eeg_files "${part_id}" "${task_id}" "${in_dir}${part_id}/${type_id}${in_file_suf}" "${out_dir}${part_id}_${type_id,,}_${task_id}${out_file_suf}";
        else
          create_nirs_files "${part_id}" "${task_id}" "${in_dir}${part_id}/${type_id}${in_file_suf}" "${out_dir}${part_id}_${type_id,,}_${task_id}${out_file_suf}";
        fi
      done
    done
  done

  printf "\n****** COMPLETED ALL TASKS FILES FOR EXP: ${exp_id} ******\n\n\n";
  log_line "****** COMPLETED ALL TASKS FILES FOR EXP: ${exp_id} ******\n\n\n";

done

printf "\n************ ALL DONE WITH EVERYTHING ************\n\n";


exit;

# EEG Data:
# create_eeg_files "lion" "rest_state" "${in_dir}lion/EEG_filtered.csv" "${out_dir}lion_eeg_rest_state.csv"
# create_eeg_files "lion" "finger_tapping" "${in_dir}lion/EEG_filtered.csv" "${out_dir}lion_eeg_finger_tapping.csv"
# create_eeg_files "lion" "affective_task_individual" "${in_dir}lion/EEG_filtered.csv" "${out_dir}lion_eeg_affective_task_individual.csv"
# create_eeg_files "lion" "affective_task_team" "${in_dir}lion/EEG_filtered.csv" "${out_dir}lion_eeg_affective_task_team.csv"
# create_eeg_files "lion" "ping_pong_competetive_0" "${in_dir}lion/EEG_filtered.csv" "${out_dir}lion_eeg_ping_pong_competetive_0.csv"
# create_eeg_files "lion" "ping_pong_cooperative_0" "${in_dir}lion/EEG_filtered.csv" "${out_dir}lion_eeg_ping_pong_cooperative_0.csv"
# create_eeg_files "lion" "hands_on_training" "${in_dir}lion/EEG_filtered.csv" "${out_dir}lion_eeg_hands_on_training.csv"
# create_eeg_files "lion" "saturn_a" "${in_dir}lion/EEG_filtered.csv" "${out_dir}lion_eeg_saturn_a.csv"
# create_eeg_files "lion" "saturn_b" "${in_dir}lion/EEG_filtered.csv" "${out_dir}lion_eeg_saturn_b.csv"

# create_eeg_files "tiger" "rest_state" "${in_dir}tiger/EEG_filtered.csv" "${out_dir}tiger_eeg_rest_state.csv"
# create_eeg_files "tiger" "finger_tapping" "${in_dir}tiger/EEG_filtered.csv" "${out_dir}tiger_eeg_finger_tapping.csv"
# create_eeg_files "tiger" "affective_task_individual" "${in_dir}tiger/EEG_filtered.csv" "${out_dir}tiger_eeg_affective_task_individual.csv"
# create_eeg_files "tiger" "affective_task_team" "${in_dir}tiger/EEG_filtered.csv" "${out_dir}tiger_eeg_affective_task_team.csv"
# create_eeg_files "tiger" "ping_pong_competetive_0" "${in_dir}tiger/EEG_filtered.csv" "${out_dir}tiger_eeg_ping_pong_competetive_0.csv"
# create_eeg_files "tiger" "ping_pong_cooperative_0" "${in_dir}tiger/EEG_filtered.csv" "${out_dir}tiger_eeg_ping_pong_cooperative_0.csv"
# create_eeg_files "tiger" "hands_on_training" "${in_dir}tiger/EEG_filtered.csv" "${out_dir}tiger_eeg_hands_on_training.csv"
# create_eeg_files "tiger" "saturn_a" "${in_dir}tiger/EEG_filtered.csv" "${out_dir}tiger_eeg_saturn_a.csv"
# create_eeg_files "tiger" "saturn_b" "${in_dir}tiger/EEG_filtered.csv" "${out_dir}tiger_eeg_saturn_b.csv"

# create_eeg_files "leopard" "rest_state" "${in_dir}leopard/EEG_filtered.csv" "${out_dir}leopard_eeg_rest_state.csv"
# create_eeg_files "leopard" "finger_tapping" "${in_dir}leopard/EEG_filtered.csv" "${out_dir}leopard_eeg_finger_tapping.csv"
# create_eeg_files "leopard" "affective_task_individual" "${in_dir}leopard/EEG_filtered.csv" "${out_dir}leopard_eeg_affective_task_individual.csv"
# create_eeg_files "leopard" "affective_task_team" "${in_dir}leopard/EEG_filtered.csv" "${out_dir}leopard_eeg_affective_task_team.csv"
# create_eeg_files "leopard" "ping_pong_competetive_1" "${in_dir}leopard/EEG_filtered.csv" "${out_dir}leopard_eeg_ping_pong_competetive_1.csv"
# create_eeg_files "leopard" "ping_pong_cooperative_0" "${in_dir}leopard/EEG_filtered.csv" "${out_dir}leopard_eeg_ping_pong_cooperative_0.csv"
# create_eeg_files "leopard" "hands_on_training" "${in_dir}leopard/EEG_filtered.csv" "${out_dir}leopard_eeg_hands_on_training.csv"
# create_eeg_files "leopard" "saturn_a" "${in_dir}leopard/EEG_filtered.csv" "${out_dir}leopard_eeg_saturn_a.csv"
# create_eeg_files "leopard" "saturn_b" "${in_dir}leopard/EEG_filtered.csv" "${out_dir}leopard_eeg_saturn_b.csv"


# NIRS Data:
# create_nirs_files "lion" "rest_state" "${in_dir}lion/NIRS_filtered.csv" "${out_dir}lion_nirs_rest_state.csv"
# create_nirs_files "lion" "finger_tapping" "${in_dir}lion/NIRS_filtered.csv" "${out_dir}lion_nirs_finger_tapping.csv"
# create_nirs_files "lion" "affective_task_individual" "${in_dir}lion/NIRS_filtered.csv" "${out_dir}lion_nirs_affective_task_individual.csv"
# create_nirs_files "lion" "affective_task_team" "${in_dir}lion/NIRS_filtered.csv" "${out_dir}lion_nirs_affective_task_team.csv"
# create_nirs_files "lion" "ping_pong_competetive_0" "${in_dir}lion/NIRS_filtered.csv" "${out_dir}lion_nirs_ping_pong_competetive_0.csv"
# create_nirs_files "lion" "ping_pong_cooperative_0" "${in_dir}lion/NIRS_filtered.csv" "${out_dir}lion_nirs_ping_pong_cooperative_0.csv"
# create_nirs_files "lion" "hands_on_training" "${in_dir}lion/NIRS_filtered.csv" "${out_dir}lion_nirs_hands_on_training.csv"
# create_nirs_files "lion" "saturn_a" "${in_dir}lion/NIRS_filtered.csv" "${out_dir}lion_nirs_saturn_a.csv"
# create_nirs_files "lion" "saturn_b" "${in_dir}lion/NIRS_filtered.csv" "${out_dir}lion_nirs_saturn_b.csv"

# create_nirs_files "tiger" "rest_state" "${in_dir}tiger/NIRS_filtered.csv" "${out_dir}tiger_nirs_rest_state.csv"
# create_nirs_files "tiger" "finger_tapping" "${in_dir}tiger/NIRS_filtered.csv" "${out_dir}tiger_nirs_finger_tapping.csv"
# create_nirs_files "tiger" "affective_task_individual" "${in_dir}tiger/NIRS_filtered.csv" "${out_dir}tiger_nirs_affective_task_individual.csv"
# create_nirs_files "tiger" "affective_task_team" "${in_dir}tiger/NIRS_filtered.csv" "${out_dir}tiger_nirs_affective_task_team.csv"
# create_nirs_files "tiger" "ping_pong_competetive_0" "${in_dir}tiger/NIRS_filtered.csv" "${out_dir}tiger_nirs_ping_pong_competetive_0.csv"
# create_nirs_files "tiger" "ping_pong_cooperative_0" "${in_dir}tiger/NIRS_filtered.csv" "${out_dir}tiger_nirs_ping_pong_cooperative_0.csv"
# create_nirs_files "tiger" "hands_on_training" "${in_dir}tiger/NIRS_filtered.csv" "${out_dir}tiger_nirs_hands_on_training.csv"
# create_nirs_files "tiger" "saturn_a" "${in_dir}tiger/NIRS_filtered.csv" "${out_dir}tiger_nirs_saturn_a.csv"
# create_nirs_files "tiger" "saturn_b" "${in_dir}tiger/NIRS_filtered.csv" "${out_dir}tiger_nirs_saturn_b.csv"

# create_nirs_files "leopard" "rest_state" "${in_dir}leopard/NIRS_filtered.csv" "${out_dir}leopard_nirs_rest_state.csv"
# create_nirs_files "leopard" "finger_tapping" "${in_dir}leopard/NIRS_filtered.csv" "${out_dir}leopard_nirs_finger_tapping.csv"
# create_nirs_files "leopard" "affective_task_individual" "${in_dir}leopard/NIRS_filtered.csv" "${out_dir}leopard_nirs_affective_task_individual.csv"
# create_nirs_files "leopard" "affective_task_team" "${in_dir}leopard/NIRS_filtered.csv" "${out_dir}leopard_nirs_affective_task_team.csv"
# create_nirs_files "leopard" "ping_pong_competetive_1" "${in_dir}leopard/NIRS_filtered.csv" "${out_dir}leopard_nirs_ping_pong_competetive_1.csv"
# create_nirs_files "leopard" "ping_pong_cooperative_0" "${in_dir}leopard/NIRS_filtered.csv" "${out_dir}leopard_nirs_ping_pong_cooperative_0.csv"
# create_nirs_files "leopard" "hands_on_training" "${in_dir}leopard/NIRS_filtered.csv" "${out_dir}leopard_nirs_hands_on_training.csv"
# create_nirs_files "leopard" "saturn_a" "${in_dir}leopard/NIRS_filtered.csv" "${out_dir}leopard_nirs_saturn_a.csv"
# create_nirs_files "leopard" "saturn_b" "${in_dir}leopard/NIRS_filtered.csv" "${out_dir}leopard_nirs_saturn_b.csv"

