# Physio data extraction:

## Package Installation
This project requires specific Python packages to be installed. Please follow the instructions below to set up the environment and install the required packages.
### Environment Setup
1. Make sure you have Python 3.11.3 installed on your system. You can download Python from the official Python website: https://www.python.org/downloads/
2. Create a new virtual environment for this project (optional but recommended). Open a terminal or command prompt and execute the following command:

```bash
python3 -m venv myenv
```
3. Activate the virtual environment. Execute the appropriate command based on your operating system:
* For Windows:
```bash
myenv\Scripts\activate
```
* For Unix or Linux:
```bash
source myenv/bin/activate
```

### Package Installation
4. Install the required packages using pip. In the activated virtual environment, execute the following command:
```bash
pip install -r requirements.txt
```

## Usage
To run the code, you need to provide the required flags:
```
python run_physio_data_extraction.py --p <path_to_xdf_files> [--pkl] [--csv] [--hdf5] [--exclude] [--output_path] [--data_validity]
```

Flags Explanation:
1. --p (Required) Enter the path to the directory containing the XDF files.
2. --pkl or --pickle: (Optional) By setting --pkl=True, the XDF iles will be extracted as pickle files. Default value is False.
3. --csv: (Optional) By setting --csv=True, the XDF files will be extracted as CSV files. Default value is True.
4. --hdf5: (Optional) By setting --hdf5=True, the XDF files will be extracted as HDF5 files. Default value is False.
5. --exclude: (Optional) Enter the name of the iMAC you'd like to exclude.
6. --output_path: (Optional) Enter the path to a folder where you want to extract the physio data. By default, the extracted files will be saved in the same folder as the XDF files. But if you'r running this on Guass then it will throw an error as the files in tomcat/raw/LangLab/ is read only. 
7. --data_validity: (Optional) Enter the csv file path which contains information about data validity per subject, task, and modality.

e.g:
```
python3 run_physio_data_extraction.py --p /tomcat/data/raw/LangLab/experiments/study_3_pilot/group/exp_2022_10_14_10/ --output_path /space/calebshibu/Neurips_new/exp_2023_07_09_12 --data_validity /space/calebshibu/data_validity.csv
```

## Wrapper script:
This script would automate the script above by going through each experiment direcory and running `run_physio_data_extraction.py`. 

## Usage
To run the script, you need to provide the following command:
```
bash wrapper_script.sh -d <root_directory> -o <output_directory>
```

Flags Explanation:
1. -d or --rootdir: (Required) Specifies the root directory path containing the directories to process.
2. -o or --outputdir: (Required) Specifies the output directory path where the script will save the output.

e.g:
```
./wrapper_script.sh -d /tomcat/data/raw/LangLab/experiments/study_3_pilot/group/ -o /space/calebshibu/Neurips_new/
```