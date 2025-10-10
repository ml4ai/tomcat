#!/bin/bash

# Application: Data Inventory
# Decription: Checks and catalogs all data files and directories for an Experiment (/path/exp_yyyy_mm_dd_hh/).
#             It checks the existents of the sub directory, file count is between min & max, file exist, and file size is between min & max.
#             Algorithm is driven by "data_inventory.tbl" and outputs to "data_inventory.log" in Experiment Dir (/path/exp_yyyy_mm_dd_hh/data_inventory.log).
# By: Rick Champlin
# Last Updated: 5/10/2023
# Start: Run Bash Script "data_inventory.sh".
#        "data_inventory.tbl" defines directories and files to check.
#        "data_inventory.vars" color variables definition used by "data_inventory.sh".

echo -ne '\033]0;Data Inventory Menu\007'

my_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
source "${my_dir}/data_inventory.vars"


# Dev Settings:
show_line_vars=false;
show_resu_line=false;


# FOR LAB:
wsl_dir=""; # This is to add to begining of bash paths, "" if running on Linux Server.
app_dir="${my_dir}/"
exp_dir="/data/cat/LangLab/experiments/study_3_pilot/group/exp_2023_05_03_10/";
dry_dir_pre="/data/cat/LangLab/dry_runs/group/exp_2023_";
plt_dir_pre="/data/cat/LangLab/experiments/study_3_pilot/group/exp_2023_";
def_file="data_inventory.tbl"; # Definition file with table of what directories and files to check for a normal ToMCAT experiment.
nar_dspl=false;
blk_dely=4; # Set how long in seconds to hold a results block on screen.
res_fnam="data_inventory.log"; # Name of Inventory Results File (Created in the root of the Experiment Directory)
res_rnam="data_inventory.run"; # Name of Inventory Results Run File (Created in the root of the Experiment Directory)
di_i=0;
di_no_menu=false;
di_pausing=false;




show_menu(){
  if ( ${di_no_menu} ); then exit; fi;

  if ( ${nar_dspl} ); then dsp_mode="NARROW"; else dsp_mode="WIDE"; fi;

  clear;

  printf "\n${menu}****************************${Red}Experiment Data Inventory${menu}****************************${NC}\n";
  printf "${menu}**${Green} Exp Dir: ${BPurple}${exp_dir}${NC}\n";
  printf "${menu}**\n";
  printf "${menu}**${number} e) ${BBlue}Set Experiment Directory.${NC}\n";
  printf "${menu}**${number} w) ${BBlue}Toggle Screen Results Width [Currently set to: ${BCyan}${dsp_mode}${BBlue}].${NC}\n";
  printf "${menu}**${number} d) ${BBlue}Set Screen Block Delay in Seconds [Currently set to: ${BCyan}${blk_dely} sec${BBlue}].${NC}\n";
  printf "${menu}**${number} c) ${BBlue}Check Data Inventory and Only Print Results on Screen.${NC}\n";
  printf "${menu}**${number} l) ${BBlue}Check Data Inventory and Create Log File in Experiment Directory.${NC}\n";
  printf "${menu}**${number} x) ${BBlue}Exit the program.${NC}\n";
  printf "${menu}*********************************************************************************${NC}\n";

  printf "Please enter a menu option and enter ${Red}(x to exit program): ${NC}";
  read opt;
}


get_exp_dir(){
  clear;
  printf "\n";
  ans="";
  while [[ "$ans" != "d" ]] && [[ "$ans" != "p" ]] && [[ "$ans" != "x" ]];
  do
    printf "${Red}Is this for a Dry-Run or Pilot?\n${Yellow}(Enter '${BItalic}${BYellow}d${NC}${Yellow}' for '${BYellow}Dry-Run${Yellow}', '${BItalic}${BYellow}p${NC}${Yellow}' for '${BYellow}Pilot${Yellow}', '${BItalic}${BRed}x${NC}${Yellow}' ${BRed}return to menu${Yellow}):${NC} ";
    read ans
  done
  if [[ "$ans" == "x" ]]; then return 1; fi
  if [[ "$ans" == "d" ]]; then new_dir=${dry_dir_pre}; else new_dir=${plt_dir_pre}; fi

  printf "\n${Red}Enter Experiment Directory: ${Yellow}(Must end with a slash '${NC}/${Yellow}')${NC}\n";
  read -e -i "${new_dir}" new_dir;
  if [[ "${new_dir: -1}" != "/" ]]; then new_dir="${new_dir}/"; fi
  tst_dir="${new_dir: -14: 10}";
  filter_date=${tst_dir//[_]/-};

  if [[ "${new_dir: -19: 5}" != "/exp_" ]]; then
    printf "\n${Yellow}The Experiment Directory that you entered does not start with '/exp_':${NC} (${BYellow}${Italic}/exp_${BGreen}yyyy_mm_dd_hh/${NC})\n";
    yes_no_question "${Red}Do you want to re-enter the Experiment Directory?${NC}";
    if [[ $? -eq 0 ]]; then
      get_exp_dir;
    fi
  fi
  if ! [[ "$filter_date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || ! date -d $filter_date >/dev/null 2>&1; then
    printf "\n${Yellow}The Date part of the Experiment Directory that you entered is Invalid:${NC} (${BGreen}/exp_${BYellow}${Italic}yyyy_mm_dd${NC}${BGreen}_hh/${NC})\n";
    yes_no_question "${Red}Do you want to re-enter the Experiment Directory?${NC}";
    if [[ $? -eq 0 ]]; then
      get_exp_dir;
    fi
  fi
  if ! [[ "${new_dir: -3: 2}" =~ ^[0-9]{2}$ ]] || [[ "${new_dir: -3: 2}" -gt 24 ]] || [[ "${new_dir: -3: 2}" -lt 1 ]]; then
    printf "\n${Yellow}The hour part of the Experiment Directory that you entered is Invalid:${NC} (${BGreen}/exp_yyyy_mm_dd_${BYellow}${Italic}hh${BGreen}/${NC})\n";
    yes_no_question "${Red}Do you want to re-enter the Experiment Directory?${NC}";
    if [[ $? -eq 0 ]]; then
      get_exp_dir;
    fi
  fi

  yes_no_question "\n${Red}Do you want set Experiment Directory to what you just entered?${NC}\n(${BYellow}${Italic}${wsl_dir}${new_dir}${NC})";
  if [[ $? -eq 0 ]]; then
    exp_dir="${wsl_dir}${new_dir}";
  fi
}


toggle_narrow_display() {
  if ( ${nar_dspl} ); then
    nar_dspl=false;
    printf "\n${BBlue}[Display Mode: ${BCyan}WIDE${BBlue}]${NC}\n";
  else
    nar_dspl=true;
    printf "\n${BBlue}[Display Mode: ${BCyan}NARROW${BBlue}]${NC}\n";
  fi
  sleep 2; 
}


set_screen_block_delay(){
  printf "\n${Red}Enter the Screen Block Delay: ${Yellow}(In Seconds)${NC}\n";
  read -e -i "${blk_dely}" blk_dely;
}


load_def_file() {
  # data_inventory.tbl
  di_i=0
  declare -a di_results
  readarray di_lines < ${def_file}
}


print_header() {
  clear;
  if ( ${nar_dspl} ); then
    printf "${BBlue}Experiment Directory: ${BPurple}${di_results[0]: -27}${di_results[2]%|*|*|*|*|*|*|*|*|*}${Blue}|${NC}\n${di_results[3]%|*|*|*|*|*|*|*|*|*}${Blue}|${NC}\n";
  else
    printf "\n${di_results[0]}${di_results[1]}${di_results[2]}${di_results[3]}";
  fi
}


create_results_file() {
  if test -f "${exp_dir}${res_fnam}" ; then
    yes_no_question "\n${Red}The Log File already exist in the Experiment Directory!${NC}\n${Red}(${BRed}${Italic}${exp_dir}${res_fnam}${Red})${NC}\n${Yellow}DO YOU WANT TO OVERWRITE IT?${NC}";
    if [[ $? -ne 0 ]]; then
      printf "\n${BRed}Saving Results Files - ABORTED!${NC}\n";
      printf "${NC}${BYellow}${Italic}"; read -n 1 -r -s -p "<Press any key to return to menu)>"; printf "${NC}";
      return 1;
    fi
  fi
  printf "\n${Blink_BGreen}Analyzing Data Inventory...${NC}\n";
  build_results_array "f";
  printf "%s\n" "${di_results[@]}" > "${exp_dir}${res_fnam}";
  build_results_array "r";
  di_results[${#di_results[@]}]='";';
  printf "%s\n" "${di_results[@]}" > "${exp_dir}${res_rnam}";
  printf "\n${BCyan}Created Results Files:${NC}\n${BGreen}${exp_dir}${res_fnam}${NC}\n${BGreen}${exp_dir}${res_rnam}${NC}\n";
  printf "${NC}${BYellow}${Italic}"; read -n 1 -r -s -p "<Press any key to return to menu)>"; printf "${NC}";
}


print_results() {
  di_x=0;
  for (( di_x=1; di_x<=1000000; di_x++ ))
  do
    printf "\n${Blink_BGreen}Analyzing Data Inventory...${NC}\n";
    build_results_array "s";
    print_header;
    di_r=0;
    for (( di_r=4; di_r<=${#di_results[@]}; di_r++ ))
    do
      di_lstr="${di_results[${di_r}]}";
      if [[ "${di_lstr: -10: 1}" == "-" ]]; then
        if ( ! ${nar_dspl} ); then printf "\n"; fi;
        if ( ${nar_dspl} ); then new_mode="WIDE"; else new_mode="NARROW"; fi;

        di_rept=true;
        while ( ${di_rept} )
        do
          if ( ${di_pausing} ); then
            printf "${Blink_Yellow}PAUSING...${NC}${BWhite}(${BBlue}${Italic}<space bar>${NC}${BWhite}=${BBlue}NEXT${BWhite},  ${Green}${Italic}<p>${NC}${BWhite}=${Green}PLAY${BWhite},  ${BCyan}${Italic}<w>${NC}${BWhite}=${BCyan}${new_mode}${BWhite},  ${Red}${Italic}<x>${NC}${BWhite}=${Red}MENU${BWhite})${NC}";
            read -s -N 1 di_input
          else
            printf "${Blink_Green}PLAYING...${NC}${BWhite}(${BBlue}${Italic}<space bar>${NC}${BWhite}=${BBlue}NEXT${BWhite},  ${Yellow}${Italic}<p>${NC}${BWhite}=${Yellow}PAUSE${BWhite},  ${BCyan}${Italic}<w>${NC}${BWhite}=${BCyan}${new_mode}${BWhite},  ${Red}${Italic}<x>${NC}${BWhite}=${Red}MENU${BWhite})${NC}";
            read -s -N 1 -t ${blk_dely} di_input
            if [[ "${di_input}" == "" ]]; then di_rept=false; continue; fi;
          fi
          if [[ "${di_input}" == " " ]]; then di_rept=false; continue; fi;
          if [[ "${di_input}" == "x" ]]; then di_rept=false; return 1; fi;
          if [[ "${di_input}" == "w" ]]; then di_rept=false; toggle_narrow_display; continue;fi;
          if [[ "${di_input}" == "p" ]]; then
            if ( ${di_pausing} ); then di_pausing=false; else di_pausing=true; fi;
            di_rept=true;
          fi
          if ( ${di_rept} ); then  printf "\r                                                                 \r"; fi;
        done

        if [[ $(( ${di_r} + 2 )) != ${#di_results[@]} ]]; then print_header; fi;
        continue;
      fi
      if ( ${nar_dspl} ) && [[ "${di_lstr: -10: 1}" != ":" ]]; then di_lstr="${di_lstr%|*|*|*|*|*|*|*|*|*}${Blue}|${NC}\n"; fi;
      printf "${di_lstr}";
    done
  done
}


load_vars() {
  #Status            |Description                                 |Directory                       |File(s)                   |Min_Size|Max_Size|Min_Count|Max_Count|File_Size|File_Count|
  #------------------|--------------------------------------------|--------------------------------|--------------------------|--------|--------|---------|---------|---------|----------|
  #1     <18>        19                 <44>                      64             <32>              97          <26>           124 <8>  133 <8>  142  <9>  152  <9>  162  <9>  172  <10>  183 

  di_stat="${di_line: 0: 18}";
  di_desc="${di_line: 19: 44}";
  di_dire="${di_line: 64: 32}";
  di_fils="${di_line: 97: 26}";
  di_mins="${di_line: 124: 8}";
  di_maxs="${di_line: 133: 8}";
  di_minc="${di_line: 142: 9}";
  di_maxc="${di_line: 152: 9}";
  di_rfls="${di_line: 162: 9}";
  di_rflc="${di_line: 172: 10}";

  di_mins_k=${di_mins/"K"/"000"};
  di_mins_m=${di_mins_k/"M"/"000000"};
  di_mins_i=${di_mins_m/"G"/"000000000"};
  di_maxs_k=${di_maxs/"K"/"000"};
  di_maxs_m=${di_maxs_k/"M"/"000000"};
  di_maxs_i=${di_maxs_m/"G"/"000000000"};

  di_minc_k=${di_minc/"K"/"000"};
  di_minc_m=${di_minc_k/"M"/"000000"};
  di_minc_i=${di_minc_m/"G"/"000000000"};
  di_maxc_k=${di_maxc/"K"/"000"};
  di_maxc_m=${di_maxc_k/"M"/"000000"};
  di_maxc_i=${di_maxc_m/"G"/"000000000"};

  di_rfls_k=${di_rfls/"K"/"000"};
  di_rfls_m=${di_rfls_k/"M"/"000000"};
  di_rfls_i=${di_rfls_m/"G"/"000000000"};
  di_rflc_k=${di_rflc/"K"/"000"};
  di_rflc_m=${di_rflc_k/"M"/"000000"};
  di_rflc_i=${di_rflc_m/"G"/"000000000"};
  
  di_rpth="${exp_dir}${di_dire}";             # Add experiment directory to sub directory. 
  di_rpth="${di_rpth//" "/""}/";              # Trim off any trailing spaces after slash ("/").
  di_path=$(ls -d ${di_rpth} 2> /dev/null);   # Resolve with wildcards in path. ("testbed_logs/*/ASR_Agent/logs").
  
  di_rfil="${di_path}${di_fils}";             # Add full path to file name.
  di_rfil="${di_rfil//" "/""}";               # Trim off any trailing spaces after slash ("/").
  if [[ "${di_fils: 0: 1}" == "*" ]]; then
    di_pfil="${di_rfil}";
  else
    di_pfil=$(ls -f ${di_rfil} 2> /dev/null); # Resolve with wildcards in file name. ("leopard_self*data.csv").
  fi
  
  di_fflt="${di_pfil##*/}";                   # Return everything after last "/".
  di_fflt="${di_fflt//" "/""}";               # Trim off any trailing spaces.
  di_fflt="${di_fflt//"*"/""}";               # Remove any "*"s.

  if ( ${show_line_vars} ); then
    printf "\n";
    printf "${BBlue}         Description: ${BGreen}${di_stat}    ${BBlue}Pre Stat: ${BGreen}${di_desc}${NC}\n";
    printf "${BBlue}           Directory: ${BGreen}${di_dire}    ${BBlue}File(s): ${BGreen}${di_fils}${NC}\n";
    printf "${BBlue}       Min File Size: ${BGreen}${di_mins}    ${BBlue}Max File Size: ${BGreen}${di_maxs}${NC}\n";
    printf "${BBlue} Min File Size Bytes: ${BGreen}${di_mins_i}    ${BBlue}Max File Size Bytes: ${BGreen}${di_maxs_i}${NC}\n";
    printf "${BBlue}      Min File Count: ${BGreen}${di_minc}    ${BBlue}Max File Count:${BGreen}${di_maxc}${NC}\n";
    printf "${BBlue}Min File Count Bytes: ${BGreen}${di_minc_i}    ${BBlue}Max File Count Bytes: ${BGreen}${di_maxc_i}${NC}\n";
    printf "${BBlue}   Resolved Dir Path: ${BGreen}${di_path}${NC}\n";
    printf "${BBlue}  Resolved File Name: ${BGreen}${di_path}${NC}\n";
    printf "${BBlue}Resolved File Filter: ${BGreen}${di_path}${NC}\n\n";
    sleep 1;
  fi
}


build_result_line() {
  # build_result_line $1="output for" ("s"=screen, "f"=plain result file, "r"=runable color result file)
  #                   $2="status flag" $3="status text" $4="status color" $5="desc color"
  #                   $6="dir color" $7="file color"
  #                   $8="min size color" $9="max size color" $10="min count color" $11="max count color"
  #                   $12="file size color" $13="file count color"
  case $1 in
    s)
      d="${Blue}|${BYellow}";
      p="${NC}${BBlue}\u25B6${BYellow}";
      di_rlin="${2}${4}${3}${p}${5}${di_desc}${d}${6}${di_dire}${d}${7}${di_fils}${d}${8}${di_mins}${d}${9}${di_maxs}${d}${10}${di_minc}${d}${11}${di_maxc}${d}${12}${di_hfls}${d}${13}${di_rflc}${d}${NC}\n";
    ;;
    f)
      d="|";
      p="|";
      di_rlin="${2}${3}${p}${di_desc}${d}${di_dire}${d}${di_fils}${d}${di_mins}${d}${di_maxs}${d}${di_minc}${d}${di_maxc}${d}${di_hfls}${d}${di_rflc}${d}";
    ;;
    r)
      d="${Blue}|${BYellow}";
      p="${NC}${BBlue}\u25B6${BYellow}";
      di_rlin="${2}${4}${3}${p}${5}${di_desc}${d}${6}${di_dire}${d}${7}${di_fils}${d}${8}${di_mins}${d}${9}${di_maxs}${d}${10}${di_minc}${d}${11}${di_maxc}${d}${12}${di_hfls}${d}${13}${di_rflc}${d}${NC}";
    ;;
  esac
  di_results[${#di_results[@]}]="${di_rlin}";
  
  if ( ${show_resu_line} ); then
    printf "\n${BBlue}Output For: ${BGreen}${1}, ${BBlue}Result Line: ${NC}\n{di_rlin}${NC}\n\n";
    sleep 1;
  fi
}


get_dir_file_count() {
  di_fcnt=$(ls "${di_path}" | grep "${di_fflt}" | wc -l); # Get Count of Files in Directory based on File Type.
  di_rflc="${di_fcnt}          ";                         # Add Trailing Spaces.
  di_rflc="${di_rflc: 0: 10}";                            # Trim for Result Column Characters Size.
}


get_last_file_size() {
  di_lfnm=$(ls ${di_path} | egrep "${di_fflt}" | tail -n 1);
  di_lfbt=$(stat --format=%s "${di_path}${di_lfnm}");
  
  di_hlfi=$(numfmt --to=iec --format="%.3f" ${di_lfbt}); # Convert file size to human format (###.#B, ###.#K, ###.#M, ###.#G)
  di_hlfs="${di_hlfi}           ";                       # Add Trailing Spaces.
  di_hlfs="${di_hlfs: 0: 9}";                            # Trim for Result Column Characters Size.
}


get_file_size() {
  di_fsiz=$(wc -c < "${di_pfil}");                       # Get File Size in Bytes.
  di_rfls="${di_fsiz}         ";                         # Add Trailing Spaces.
  di_rfls="${di_rfls: 0: 9}";                            # Trim for Result Column Characters Size.

  di_hsiz=$(numfmt --to=iec --format="%.3f" ${di_fsiz}); # Convert file size to human format (###.###B, ###.###K, ###.###M, ###.###G) 
  di_hfls="${di_hsiz}         ";                         # Add Trailing Spaces.
  di_hfls="${di_hfls: 0: 9}";                            # Trim for Result Column Characters Size.
}


build_results_array() {
  # build_results_array $1="output for" ("s"=screen, "f"=plain result file, "r"=runable color result file)
  load_def_file;
  di_results=();
  case $1 in
    s)
      di_results[${#di_results[@]}]="${BBlue}Experiment Directory: ${BPurple}${exp_dir}${NC}\n";
      di_results[${#di_results[@]}]="${Blue}${di_lines[1]}${NC}";
      di_results[${#di_results[@]}]="${Blue}${di_lines[2]}${NC}";
      di_results[${#di_results[@]}]="${Blue}${di_lines[3]}${NC}";
      di_xflg="${Red_x} ";
      di_wflg="${Yellow_Warn}";
      di_oflg="${Green_Check} ";
    ;;
    f)
      di_results[${#di_results[@]}]="Experiment Directory: ${exp_dir}";
      for (( di_h=1; di_h<=3; di_h++ ))
      do
        di_line="${di_lines[${di_h}]}";
        di_line=${di_line//$'\r'};
        di_line=${di_line//$'\n'};
        di_results[${#di_results[@]}]="${di_line}";
      done
      di_xflg="X ";
      di_wflg="∆ ";
      di_oflg="√ ";
    ;;
    r)
      di_results[${#di_results[@]}]='clear; printf "'${BBlue}'Experiment Directory: '${BPurple}${exp_dir}${NC};
      for (( di_h=1; di_h<=3; di_h++ ))
      do
        di_line="${di_lines[${di_h}]}";
        di_line=${di_line//$'\r'};
        di_line=${di_line//$'\n'};
        di_results[${#di_results[@]}]=${Blue}${di_line}${NC};
      done
      di_xflg="${Red_x} ";
      di_wflg="${Yellow_Warn}";
      di_oflg="${Green_Check} ";
    ;;
  esac

  for (( di_i=4; di_i<="${#di_lines[@]}"; di_i++ ))
  do
    di_line="${di_lines[di_i]}";

    if [[ "${di_line: 18: 1}" == "|" ]]; then
      # This is NOT a Block Title Line...
      load_vars;
      if [ -d "${di_path}" ]; then
        # Directory Exist...
        if [[ "${di_fils: 0: 1}" == "*" ]]; then
          # This is a Files in Directory Check...
          get_dir_file_count;
          if [[ "${di_fcnt}" == "0" ]]; then di_hfls="(0 files)"; build_result_line "${1}" "${di_xflg}" "Dir Has No Files" "${Black}${On_Red}" "${Red}" "${Red}" "" "" "" "${BRed}" "" "${BRed}" "${BRed}"; continue; fi;
          get_last_file_size;
          di_hfls="${di_hlfs}";
          if [[ ${di_fcnt} -lt ${di_minc_i} ]]; then build_result_line "${1}" "${di_wflg}" "File Count < Min" "${Black}${On_Yellow}" "${Yellow}" "" "" "" "" "${BRed}" "" "" "${BRed}"; continue; fi;
          if [[ ${di_fcnt} -gt ${di_maxc_i} ]]; then build_result_line "${1}" "${di_wflg}" "File Count > Max" "${Black}${On_Yellow}" "${Yellow}" "" "" "" "" "" "${BRed}" "" "${BRed}"; continue; fi;
          if [[ "${di_lfbt}" == "0" ]]; then build_result_line "${1}" "${di_xflg}" "Ls-Fl Has 0 Byts" "${Black}${On_Red}" "${Red}" "${BGreen}" "" "${BRed}" "${BRed}" "${BGreen}" "${BGreen}" "${BRed}" "${BGreen}" ; continue; fi;
          if [[ ${di_lfbt} -lt ${di_mins_i} ]]; then build_result_line "${1}" "${di_wflg}" "Ls-Fl Size < Min" "${Black}${On_Yellow}" "${Yellow}" "${BGreen}" "${BGreen}" "${BRed}" "" "${BGreen}" "${BGreen}" "${BRed}" "${BGreen}"; continue; fi;
          if [[ ${di_lfbt} -gt ${di_maxs_i} ]]; then build_result_line "${1}" "${di_wflg}" "Ls-Fl Size > Max" "${Black}${On_Yellow}" "${Yellow}" "${BGreen}" "${BGreen}" "" "${BRed}" "${BGreen}" "${BGreen}" "${BRed}" "${BGreen}"; continue; fi;
          build_result_line "${1}" "${di_oflg}" "Dir Files Are OK" "${Black}${On_Green}" "${Green}" "${BGreen}" "${BGreen}" "${BGreen}" "${BGreen}" "${BGreen}" "${BGreen}" "${BGreen}" "${BGreen}"; continue; 

        else
          # This is a Individual File Check...
          if test -f "${di_pfil}" ; then
            # File Exist...
            get_dir_file_count;
            get_file_size;
            if [[ "${di_fsiz}" == "0" ]]; then build_result_line "${1}" "${di_xflg}" "File Has 0 Bytes" "${Black}${On_Red}" "${Red}" "${BGreen}" "" "${BRed}" "${BRed}" "${BGreen}" "${BGreen}" "${BRed}" "${BGreen}" ; continue; fi;
            if [[ ${di_fsiz} -lt ${di_mins_i} ]]; then build_result_line "${1}" "${di_wflg}" "File Size < Min " "${Black}${On_Yellow}" "${Yellow}" "${BGreen}" "${BGreen}" "${BRed}" "" "${BGreen}" "${BGreen}" "${BRed}" "${BGreen}"; continue; fi;
            if [[ ${di_fsiz} -gt ${di_maxs_i} ]]; then build_result_line "${1}" "${di_wflg}" "File Size > Max " "${Black}${On_Yellow}" "${Yellow}" "${BGreen}" "${BGreen}" "" "${BRed}" "${BGreen}" "${BGreen}" "${BRed}" "${BGreen}"; continue; fi;
            build_result_line "${1}" "${di_oflg}" "File Is OK      " "${Black}${On_Green}" "${Green}" "${BGreen}" "${BGreen}" "${BGreen}" "${BGreen}" "${BGreen}" "${BGreen}" "${BGreen}" "${BGreen}"; continue;

          else
            # File does NOT Exist...
            di_rfls="(no file)";
            di_rflc="(no file) ";
            build_result_line "${1}" "${di_xflg}" "File Not Exist  " "${Black}${On_Red}" "${Red}" "${BGreen}" "${BRed}" "" "" "${BRed}" "${BRed}" "${BRed}" "${BRed}"; continue;
          fi
        fi

      else
        # Directory does NOT Exist...
        di_hfls="(no dir) ";
        di_rflc="(no dir)  ";
        build_result_line "${1}" "${di_xflg}" "Dir Not Exist   " "${Black}${On_Red}" "${Red}" "${BRed}" "" "" "" "" "" "${BRed}" "${BRed}"; continue;
      fi

    else
      # This is a Block Title Line...
      if [[ "${1}" == "s" ]]; then
        di_line="${di_line/':'/':'${Blue}}";
        di_results[${#di_results[@]}]="${BBlue}${di_line}${NC}";
        continue;
      fi
      # di_line="Rick Test:"
      if [[ "${1}" == "f" ]]; then
        di_line=${di_line//$'\r'};
        di_line=${di_line//$'\n'};
        di_results[${#di_results[@]}]="${di_line}";
        continue;
      fi
      if [[ "${1}" == "r" ]]; then
        if [[ "${di_line: -3: 1}" == "-" ]]; then
          if [[ $(( ${di_i} + 1 )) != ${#di_lines[@]} ]]; then
            di_line=${NC}${BYellow}${Italic}'"; read -n 1 -r -s -p "<Press any key for Next Block>"; clear; printf "'${NC};
            di_results[${#di_results[@]}]=${di_line}${BBlue}'Experiment Directory: '${BPurple}${exp_dir}${NC};
            for (( di_h=1; di_h<=3; di_h++ ))
            do
              di_line="${di_lines[${di_h}]}";
              di_line=${di_line//$'\r'};
              di_line=${di_line//$'\n'};
              di_results[${#di_results[@]}]=${Blue}${di_line}${NC};
            done
          fi
        else 
          di_line="${di_line/':'/':'${Blue}}";
          di_line=${di_line//$'\r'};
          di_line=${di_line//$'\n'};
          di_results[${#di_results[@]}]=${BBlue}${di_line}${NC};
        fi
        continue;
      fi
    fi
  done
}


show_menu;

while [ "$opt" != '' ]
  do
  case $opt in
    e) get_exp_dir;
       show_menu;
    ;;
    w) toggle_narrow_display;
       show_menu;
    ;;
    d) set_screen_block_delay;
       show_menu;
    ;;
    c) print_results;
       show_menu;
    ;;
    l) create_results_file;
       show_menu;
    ;;
    x)exit;
    ;;
    \n)exit;
    ;;
    *) printf "\n${Red}*** Not a valid option ***${NC}\n";
      sleep 3;
      show_menu;
    ;;
  esac
done

exit;
