# Signal filtering

This project is for filtering EEG and fNIRS signals and add them to the database

## Install dependencies

This project requires Python 3.11.

### Install dependencies with Conda

Ensure that Conda is already installed, then run the following command to install dependencies into a new conda environment

```
conda conda create -n tomcat-filter python=3.11
conda activate tomcat-filter
pip install -r requirements.txt
```

### Install dependencies with Pip

You can install the packages using Pip3 in a base environment or an environment created for this project, then run the follow command to install dependencies

```
pip3 install -r requirements.txt
```

### Install dependencies manually

If you want to set up environment and install dependencies manually, then you need the following packages

```
numpy
scipy
pandas
scikit-learn
mne
tqdm
python-dotenv
python-dateutil
```

## Set up project

Ensure that you downloaded the ToMCAT raw data [here](tomcat.ivilab.org)

Set the following variables in the `config.py` file:

- `DB_PATH` is the full path to the ToMCAT data you downloaded.
- `EEG_RAW_PATH` is the full path to the ToMCAT raw EEG data.
- `EEG_FILTERED_PATH` is the full path where the filtered EEG data will be placed after running the program.
- `FNIRS_FILTERED_PATH` is the full path where the filtered fNIRS data will be placed after running the program.
- `NUM_PROCESSES` number of processes you can use to run the data processing in parallel.

## Run project

After setting up the `config.py` file, you can launch the program (make sure that you are in the python environment with required dependencies installed):

### Filter fNIRS data

```
python3 process_nirs.py
```

### Filter EEG data

```
python3 process_eeg.py
```

## Output

The program will output synchronized data to the path specified in `EEG_FILTERED_PATH` and `FNIRS_FILTERED_PATH` in `config.py`.

