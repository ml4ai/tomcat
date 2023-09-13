from utils import cd, is_directory_with_unified_xdf_files
import logging
from logging import info, error
import argparse
from config import EXP_DIR, OUT_DIR, USER
import sys
import os
from entity.pcm_audio import PCMAudio
from tqdm import tqdm
import wave
import contextlib
import pandas as pd
from scipy.io import wavfile
import numpy as np
from typing import Callable
from glob import glob

# Replace the variable below with a list of predefined filenames if you want to regenerate the report just for the
# filenames in the list and in that order.
USE_FILES = None

# Number of seconds in the beginning of each audio considered as noise for the signal-to-noise ratio calculation.
NOISE_SECONDS = 5


def duration_format(duration: float):
    seconds = int(duration)
    minutes = int(seconds / 60)
    seconds = seconds % 60

    if minutes < 10:
        minutes = f"0{minutes}"

    if seconds < 10:
        seconds = f"0{seconds}"

    return f"{minutes}:{seconds}"


def save_summary(experiments_dir: str, out_dir: str):
    with cd(experiments_dir):
        directories_to_process = [directory for directory in os.listdir(".") if directory[:4] == "exp_"]

        entries = []
        for group_session in tqdm(sorted(directories_to_process), unit="directories"):
            if not is_directory_with_unified_xdf_files(group_session):
                entries.extend(process_directory_v1(group_session))
            else:
                entries.extend(process_directory_v2(group_session))

        df = pd.DataFrame(data=entries, columns=["audio", "size", "duration (seconds)", "duration", "volume", "noise"])
        if USE_FILES:
            df.audio = df.audio.astype("category")
            df.audio = df.audio.cat.set_categories(USE_FILES)  # Sort in the same order as the file list
        df = df.sort_values(["audio"])

        os.makedirs(out_dir, exist_ok=True)
        df.to_csv(f"{out_dir}/audio_summary.csv")


def process_directory_v1(group_session: str):
    return process_directory(group_session, lambda g, s: f"{g}/{s}/audio")


def process_directory_v2(group_session: str):
    return process_directory(group_session, lambda g, s:  f"{g}/{s}/audio/block_2")


def process_directory(group_session: str, audio_dir_fn: Callable):
    entries = []
    for station in ["lion", "tiger", "leopard"]:
        audio_dir = audio_dir_fn(group_session, station)
        if not os.path.exists(audio_dir):
            print(f"Audio folder does not exist for station {station} in group session {group_session}.")
            continue

        audio_files = glob(f"{audio_dir}/*.wav")
        for audio_filepath in audio_files:
            audio_filename = os.path.basename(audio_filepath)
            if not USE_FILES or audio_filename in USE_FILES:
                file_size_in_mb = os.path.getsize(audio_filepath) / (1024 * 1024)

                with contextlib.closing(wave.open(audio_filepath, 'r')) as f:
                    frames = f.getnframes()
                    rate = f.getframerate()
                    duration = frames / float(rate)

                samplerate, data = wavfile.read(audio_filepath)
                audible = not (data == 0).all()

                noise_start = 0  # Adjust this to the start of the noise segment
                noise_end = 5 * samplerate  # Adjust this to the end of the noise segment
                noise_segment = data[noise_start:noise_end]
                rms_noise = np.sqrt(np.mean(np.square(noise_segment)))
                rms_audio = np.sqrt(np.mean(np.square(data)))

                # Signal-to-noise ratio (SNR) in decibels (dB):
                snr_db = 20 * np.log10(rms_audio / rms_noise)

                entries.append([audio_filename, f"{file_size_in_mb:.1f}MB", duration, duration_format(duration),
                                "Audible" if audible else "Inaudible", snr_db])

    return entries


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parses a collection of experiments, finds the audio files, and saves a series of info to a"
                    " .csv file."
    )

    parser.add_argument("--experiments_dir", type=str, required=False, default=EXP_DIR,
                        help="Directory containing experiment folders.")
    parser.add_argument("--out_dir", type=str, required=True,
                        help="Directory where summary must be saved.")

    args = parser.parse_args()

    save_summary(args.experiments_dir, args.out_dir)
