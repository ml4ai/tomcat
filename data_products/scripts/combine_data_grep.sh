#!/bin/bash

my_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"


# Dev Settings:
show_eeg_data_checks=false;
show_nir_data_checks=false;


# On gauss:
in_root_dir="/space/calebshibu/Neurips_new_bug_fix/";
out_root_dir="/space/rchamplin/Neurips/rerun_2023_06_02/";

# At Home
# in_root_dir="/mnt/c/Users/rcham/Documents/ToMCAT/Lab/combine_data/space/calebshibu/Neurips/";
# out_root_dir="/mnt/c/Users/rcham/Documents/ToMCAT/Lab/combine_data/space/rchamplin/Neurips/grep/";

log_file="combine_data.log";

experiments="exp_2023_02_21_14 exp_2022_09_09_12 exp_2022_09_29_15 exp_2022_09_30_10 exp_2022_10_04_09 exp_2022_10_07_15 exp_2022_10_14_10 exp_2022_10_18_10 exp_2022_10_21_15 exp_2022_10_24_12 exp_2022_10_27_10 exp_2022_10_28_10 exp_2022_10_31_10 exp_2022_11_01_10 exp_2022_11_04_10 exp_2022_11_07_10 exp_2022_11_08_11 exp_2022_11_10_10 exp_2022_11_14_12 exp_2022_11_15_13 exp_2022_11_17_15 exp_2022_11_18_10 exp_2022_11_22_10 exp_2022_12_02_15 exp_2022_12_05_12 exp_2023_01_30_13 exp_2023_01_31_14 exp_2023_02_03_10 exp_2023_02_06_13 exp_2023_02_07_14 exp_2023_02_10_10 exp_2023_02_16_14 exp_2023_02_20_01";
# experiments="exp_2023_02_21_14";

participants="lion tiger leopard";

types="EEG NIRS";

tasks="rest_state finger_tapping affective_task_individual affective_task_team ping_pong_competetive_0 ping_pong_competetive_1 ping_pong_cooperative_0 hands_on_training saturn_a saturn_b";

in_file_suf="_filtered.csv"; # ${type_id}${in_file_suf}  Example: EEG_filtered.csv
out_file_suf=".csv"; # ${part_id}_${type_id,,}_${task_id}${out_file_suf}  Example: lion_eeg_rest_state.csv

# Code needed if it is decided to have unique header rows:
# eval od="\t";
# eval eeg_header="line${od}AFF1h${od}F7${od}FC5${od}C3${od}T7${od}TP9${od}Pz${od}P3${od}P7${od}O1${od}O2${od}P8${od}P4${od}TP10${od}Cz${od}C4${od}T8${od}FC6${od}FCz${od}F8${od}AFF2h${od}AUX_GSR${od}AUX_EKG${od}human_readable_time${od}unix_time${od}event_type\n";
# eval nirs_header="line${od}S1_D1_HbO${od}S1_D2_HbO${od}S2_D1_HbO${od}S2_D3_HbO${od}S3_D1_HbO${od}S3_D3_HbO${od}S3_D4_HbO${od}S4_D2_HbO${od}S4_D4_HbO${od}S4_D5_HbO${od}S5_D3_HbO${od}S5_D4_HbO${od}S5_D6_HbO${od}S6_D4_HbO${od}S6_D6_HbO${od}S6_D7_HbO${od}S7_D5_HbO${od}S7_D7_HbO${od}S8_D6_HbO${od}S8_D7_HbO${od}S1_D1_HbR${od}S1_D2_HbR${od}S2_D1_HbR${od}S2_D3_HbR${od}S3_D1_HbR${od}S3_D3_HbR${od}S3_D4_HbR${od}S4_D2_HbR${od}S4_D4_HbR${od}S4_D5_HbR${od}S5_D3_HbR${od}S5_D4_HbR${od}S5_D6_HbR${od}S6_D4_HbR${od}S6_D6_HbR${od}S6_D7_HbR${od}S7_D5_HbR${od}S7_D7_HbR${od}S8_D6_HbR${od}S8_D7_HbR${od}human_readable_time${od}unix_time${od}event_type\n";
# printf "\nEEG Head: ${eeg_header}\n\n\nNIRS Head: ${nirs_header}\n";

out_first_row_header=true;

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
  la="${out_root_dir}${log_file}";
  lf="${out_dir}${log_file}";
  if [[ "${1}" == "RESET" ]]; then
    if test -f "${lf}" ; then
      rm "${lf}";
    fi
  else
    if [ ! -f "${la}" ]; then
      touch "${la}";
    fi
    if [ ! -f "${lf}" ]; then
      touch "${lf}";
    fi
    printf "${1}" >> "${la}";
    printf "${1}" >> "${lf}";
  fi
}


create_out_file(){
  # Passed in argument examples:
  # $1 = part_name "Lion";
  # $2 = task_name "ping_pong_competetive_0";
  # $3 = type_name "EEG" or "NIRS";
  # $4 = in_file "${in_dir}lion/EEG_filtered.csv";
  # $5 = out_file "${out_dir}lion_eeg_affective_task_individual.csv"";

  part_name="${1}";
  type_name="${2}";
  task_name="${3}";
  in_file="${4}";
  out_file="${5}";

  # If the output dir does not exist, create it. 
  if [ ! -d "${out_dir}" ]; then
    mkdir -p ${out_dir}
  fi

  # Log the process start of creating output file for Experiment / Participant / Data Type / Task.
  dt=$(date +%Y-%m-%d_%H:%M:%S);
  printf "${dt} *** STARTING - EXP: ${exp_id}  For: ${part_name}  Type: ${type_name}  Task: ${task_name}\n      In File: ${in_file}\n      Out File: ${out_file}\n";
  log_line "${dt} *** STARTING - EXP: ${exp_id}  For: ${part_name}  Type: ${type_name}  Task: ${task_name}\n      In File: ${in_file}\n      Out File: ${out_file}\n";

  # If the input file does not exist, log/print error and jump out of sub routine. 
  if ! test -f "${in_file}" ; then
    dt=$(date +%Y-%m-%d_%H:%M:%S);
    printf "${dt} *** ERROR: Input File Does Not Exist - EXP: ${exp_id}  For: ${part_name}  Type: ${type_name}  Task: ${task_name}\n      In File: ${in_file}\n\n\n";
    log_line "${dt} *** ERROR: Input File Does Not Exist - EXP: ${exp_id}  For: ${part_name}  Type: ${type_name}  Task: ${task_name}\n      In File: ${in_file}\n\n\n";
    return 0;
  fi

  # If output file previously exists, delete it. 
  if test -f "${out_file}" ; then
    rm "${out_file}";
  fi

  # Get count of how many data lines/rows exist in the input file for task being processed.
  found_count=0;
  found_count=$(grep -c ${task_name} ${in_file})

  if [ ${found_count} -eq 0 ]; then
    # No data lines/rows exist in the input file for task being processed, log/print and exit routine.
    dt=$(date +%Y-%m-%d_%H:%M:%S);
    printf "${dt} *** ERROR: No Records Found - EXP: ${exp_id}  For: ${part_name}  Type: ${type_name}  Task: ${task_name}\n      In File: ${in_file}\n\n\n";
    log_line "${dt} *** ERROR: No Records Found - EXP: ${exp_id}  For: ${part_name}  Type: ${type_name}  Task: ${task_name}\n      In File: ${in_file}\n\n\n";
    return 0;

  else
    if ( ${out_first_row_header} ); then
      # Lines/rows found for task being processed, create output file with header.
      grep "${task_name}\|event_type" "${in_file}" > "${out_file}"

      # Code notes for if I need to create unique header row, not needed now.  
      #   if [[ "${type_name}" == "EEG" ]]; then out_line="${eeg_header}"; else out_line="${nirs_header}"; fi
      #   printf "${out_line}" >> "${out_file}";
      #   grep "${task_name}" "${in_file}" > "${out_file}"

    else
      # Lines/rows found for task being processed, create output file WITHOUT header.
      grep "${task_name}" "${in_file}" > "${out_file}"

    fi

    # Process of creating output file for Experiment / Participant / Data Type / Task - Completed, log/print. 
    dt=$(date +%Y-%m-%d_%H:%M:%S); 
    printf "${dt} *** COMPLETED - EXP: ${exp_id}  For: ${part_name}  Type: ${type_name}  Task: ${task_name}  Found Lines: ${found_count}\n\n\n";
    log_line "${dt} *** COMPLETED - EXP: ${exp_id}  For: ${part_name}  Type: ${type_name}  Task: ${task_name}  Found Lines: ${found_count}\n\n\n";

  fi 

}



# Arguments passed to create_out_file() examples:
# $1 = part_name "Lion";
# $2 = type_name "EEG" or "NIRS";
# $3 = task_name "ping_pong_competetive_0";
# $4 = in_file "${in_dir}lion/EEG_filtered.csv";
# $5 = out_file "${out_dir}lion_eeg_affective_task_individual.csv"";

# Examples of the predefined lists:
# experiments="exp_2023_02_21_14 exp_2022_09_09_12 exp_2022_09_29_15";
# participants="lion tiger leopard";
# types="EEG NIRS";
# tasks="rest_state finger_tapping affective_task_individual affective_task_team ping_pong_competetive_0 ping_pong_competetive_1 ping_pong_cooperative_0 hands_on_training saturn_a saturn_b";


# Start of loops to process all files for Experiments / Participants / Data Types / Tasks: 
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
        create_out_file "${part_id}" "${type_id}" "${task_id}" "${in_dir}${part_id}/${type_id}${in_file_suf}" "${out_dir}${part_id}_${type_id,,}_${task_id}${out_file_suf}";
      done
    done
  done

  # Process completed for processing all file for the Experiment, log/print.
  dt=$(date +%Y-%m-%d_%H:%M:%S);
  printf "${dt} ****** COMPLETED ALL TASKS FILES FOR EXP: ${exp_id} ******\n\n\n";
  log_line "${dt} ****** COMPLETED ALL TASKS FILES FOR EXP: ${exp_id} ******\n\n\n";

done

# All Experiments done, log/print, program done, exit.
dt=$(date +%Y-%m-%d_%H:%M:%S);
printf "${dt} ************ ALL DONE WITH EVERYTHING ************\n\n";


exit;
