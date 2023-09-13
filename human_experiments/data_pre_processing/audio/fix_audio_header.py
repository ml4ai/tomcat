from utils import cd, is_directory_with_unified_xdf_files
import logging
from logging import info, error
import argparse
from common.config import EXP_DIR, OUT_DIR, USER, LOG_DIR
import sys
import os
from audio.entity.pcm_audio import PCMAudio
from tqdm import tqdm

"""
Some audio files do not contain the Subchunk2Size part of the PCM-format header filled, which causes the file not
to be recognized by the apple audio player or even python libraries to read wave files. This script completes the
Subchunk2Size data in the header of the audio files according to what it is supposed to contain as described in
http://soundfile.sapp.org/doc/WaveFormat/
"""

def fix_audio_header(experiments_dir: str, out_dir: str):
    info("Processing directories...")

    directories_to_process = [directory for directory in os.listdir(experiments_dir) if
                              os.path.basename(directory)[:4] == "exp_"]

    for group_session in tqdm(sorted(directories_to_process), unit="directories"):
        info(f"Processing group session {group_session}")
        experiment_dir = f"{experiments_dir}/{group_session}"

        if not is_directory_with_unified_xdf_files(experiment_dir):
            process_directory_v1(experiment_dir, out_dir)
        else:
            process_directory_v2(experiment_dir, out_dir)


def process_directory_v1(experiment_dir: str, out_dir: str):
    return process_directory(experiment_dir, out_dir, lambda g, s: f"{g}/{s}/audio")


def process_directory_v2(experiment_dir: str, out_dir: str):
    return process_directory(experiment_dir, out_dir, lambda g, s: f"{g}/{s}/audio/block_2")


def process_directory(experiment_dir: str, out_dir: str, audio_dir_fn: Callable):
    for station in ["lion", "tiger", "leopard"]:
        audio_dir = audio_dir_fn(experiment_dir, station)
        if not os.path.exists(audio_dir):
            error(
                f"Audio folder does not exist for station {station} in group session {os.path.basename(experiment_dir)}.")
            continue

        for audio_file in os.listdir(audio_dir):
            audio = PCMAudio(filepath=f"{audio_dir}/{audio_file}")

            os.makedirs(f"{out_dir}/{audio_dir}", exist_ok=True)
            audio.fix_header(out_filepath=f"{out_dir}/{audio_dir}/{audio_file}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parses a collection of experiments, finds the audio files, fix their header by filling the"
                    " Subchunk2Size portion and save them to the output folder, maintaining the folder structure"
                    " in the original experiment."
    )

    parser.add_argument("--experiments_dir", type=str, required=False, default=EXP_DIR,
                        help="Directory containing experiment folders.")
    parser.add_argument("--out_dir", type=str, required=False, default=OUT_DIR,
                        help="Directory where experiment folder structure containing fixed audios must be saved.")
    parser.add_argument("--log_dir", type=str, required=False, default=LOG_DIR,
                        help="Directory where log files must be saved.")

    args = parser.parse_args()

    os.makedirs(args.log_dir, exist_ok=True)
    logging.basicConfig(
        level=logging.INFO,
        handlers=(
            logging.FileHandler(
                filename=f"{args.log_dir}/fix_audio_header.log",
                mode="w",
            ),
            logging.StreamHandler(stream=sys.stderr),
        ),
    )

    fix_audio_header(args.experiments_dir, args.out_dir)
