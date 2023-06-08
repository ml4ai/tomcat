# Physio data extraction for old data acquisition pipeline:

## Usage
To run the code, you need to provide the required flags:
```
python script.py --p1 <path_to_xdf_files> --p2 <path_to_baseline_task_data> --p3 <path_to_minecraft_data> --s <subject_id> [additional_flags]
```

Flags Explanation:
1. --p1 or --path1: (Required) Path to the directory with the XDF files.
2. --p2 or --path2: (Required) Path to the folder with baseline task data.
3. --p3 or --path3: (Required) Path to the folder with Minecraft data.
4. --s or --subject_id: (Required) Path to the folder with the baseline task data. This flag can be used multiple times to provide multiple subject IDs.
5. --pkl or --pickle: (Optional) By setting --pkl=True, the XDF files will be extracted as pickle files. Default value is False.
6. --csv: (Optional) By setting --csv=True, the XDF files will be extracted as CSV files. Default value is True.
7. --hdf5: (Optional) By setting --hdf5=True, the XDF files will be extracted as HDF5 files. Default value is False.
8. --exclude: (Optional) Enter the name of the iMAC you'd like to exclude.
9. --filter: (Optional) Enter True if you want to filter the signal.
10. --output_path: (Optional) Enter the path to a folder where you want to extract the physio data. By default, the extracted files will be saved in the same folder as the XDF files. But if you'r running this on Guass then it will throw an error as the files in tomcat/raw/LangLab/ is read only. 

e.g:
```
python3 run_physio_data_extraction.py --p1 /tomcat/data/raw/LangLab/experiments/study_3_pilot/group/exp_2022_10_14_10/ --p2 /tomcat/data/raw/LangLab/experiments/study_3_pilot/group/exp_2022_10_14_10/baseline_tasks/ --p3 /tomcat/data/raw/LangLab/experiments/study_3_pilot/group/exp_2022_10_14_10/minecraft/ --s 00039 --s 00041 --s 44 --output_path /space/calebshibu/Neurips_new/exp_2022_10_14_10 --filter True
```

## Wrapper script:
This script would automate the script above by going through each experiment direcory and running `run_physio_data_extraction.py`. 

## Usage
To run the script, you need to provide the following command:
```
bash script.sh -d <root_directory> -o <output_directory>
```

Flags Explanation:
1. -d or --rootdir: (Required) Specifies the root directory path containing the directories to process.
2. -o or --outputdir: (Required) Specifies the output directory path where the script will save the output.

e.g:
```
./wrapper_script.sh -d /tomcat/data/raw/LangLab/experiments/study_3_pilot/group/ -o /space/calebshibu/Neurips_new/
```