import argparse
import logging
import os
import sys
from logging import info, error
from typing import Callable

from tqdm import tqdm

from audio.entity.pcm_audio import PCMAudio
from common.config import EXP_DIR, OUT_DIR, LOG_DIR
from utils import cd, is_directory_with_unified_xdf_files


def extract_vocalic_features(experiments_dir: str, out_dir: str):
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
            df = audio.extract_vocalic_features()

            # Replace pcm_ with wave_ to match column name with CLI extraction
            # df = df.drop(columns=["file", "start", "end"])
            df.columns = [f"wave_{col[3:]}" if col[3:] == "pcm" else col for col in df.columns]

            vocalic_filename = audio_file[:audio_file.rfind(".")] + "_2.csv"
            sub_dir = audio_dir[audio_dir.find("exp_"):]

            os.makedirs(f"{out_dir}/{sub_dir}", exist_ok=True)
            df.to_csv(f"{out_dir}/{sub_dir}/{vocalic_filename}", sep=";")

            sys.exit()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parses a collection of experiments, finds the audio files, extracts vocalic features from the audio "
                    " and save them to a .csv file."
    )

    parser.add_argument("--experiments_dir", type=str, required=False, default=EXP_DIR,
                        help="Directory containing experiment folders.")
    parser.add_argument("--out_dir", type=str, required=False, default=OUT_DIR,
                        help="Directory where experiment folder structure containing vocalic features files must be saved.")
    parser.add_argument("--log_dir", type=str, required=False, default=LOG_DIR,
                        help="Directory where log files must be saved.")

    args = parser.parse_args()

    os.makedirs(args.log_dir, exist_ok=True)
    logging.basicConfig(
        level=logging.INFO,
        handlers=(
            logging.FileHandler(
                filename=f"{args.log_dir}/extract_vocalic_features.log",
                mode="w",
            ),
            logging.StreamHandler(stream=sys.stderr),
        ),
    )

    extract_vocalic_features(args.experiments_dir, args.out_dir)
