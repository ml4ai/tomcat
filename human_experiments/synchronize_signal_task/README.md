# Synchronize signal task

This project is for synchronizing EEG, EKG, GSR, and fNIRS signals with task data.

## Install dependencies

This project requires Python 3.11.

### Install dependencies with Conda

Ensure that Conda is already installed, then run the following command to install dependencies into a new conda environment

```
conda conda create -n tomcat-synchronize python=3.11
conda activate tomcat-synchronize
pip install -r requirements.txt
```

### Install dependcies with Pip

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
- `EEG_FILTERED_PATH` is the full path to the ToMCAT filtered EEG data.
- `FNIRS_FILTERED_PATH` is the full path to the ToMCAT filtered fNIRS data.
- `NUM_PROCESSES` number of processes you can use to run the data processing in parallel.
- `OUTPUT_DIR` the full path to where the output will be located.

## Run project

After setting up the `config.py` file, you can launch the program (make sure that you are in the python environment with required dependencies installed):

### Synchronize fNIRS with each task at 10 Hz

```
python3 process_nirs_10hz.py
```

### Synchronize EEG-EKG-GSR with each task at 500 Hz

```
python3 process_eeg_500hz.py
```

## Output

The program will output synchronized data to the path specified in `output_dir` in `config.py`.
