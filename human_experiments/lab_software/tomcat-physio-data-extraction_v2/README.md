# Extraction for old data acquisition pipeline:

## Usage 
To run the code, you need to provide the required flags:
To run the code, you need to provide the required flags:
```
python3 run_physio_data_extraction_v2.py --p <path_to_xdf_files> [--pkl <True/False>] [--csv <True/False>] [--hdf5 <True/False>] [--exclude <iMAC_name>] [--filter <True/False>] [--output_path <output_directory>]
```

Flags Explanation:
1. `--p <path_to_xdf_files>`: Required. Path to the directory containing the XDF files.
2. `--pkl <True/False>`: Set to True to extract XDF files as pickle files. Default is False.
3. `--csv <True/False>`: Set to True to extract XDF files as CSV files. Default is True.
4. `--hdf5 <True/False>`: Set to True to extract XDF files as HDF5 files. Default is False.
5. `--exclude <iMAC_name>`: Specify the iMAC name you want to exclude from processing.
6. `--filter <True/False>`: Set to True if you want to filter the signal.
7. `--output_path <output_directory>`: Specify the output directory where you want to extract the physio data. By default, the data will be extracted to the same folder as the XDF files.

e.g: 
```
python3 run_physio_data_extraction_v2.py --p /tomcat/data/raw/LangLab/experiments/study_3_pilot/group/exp_2023_05_03_10 --filter True --output_path /tomcat/data/derived/drafts
```