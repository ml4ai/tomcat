from utils import cd, is_directory_with_unified_xdf_files
import logging
from logging import info, error
import argparse
from common.config import EXP_DIR, OUT_DIR, USER, LOG_DIR
import sys
import os
from audio.entity.pcm_audio import PCMAudio
from tqdm import tqdm


def extract_vocalic_features(experiments_dir: str, out_dir: str):
    info("Processing directories...")

    with cd(experiments_dir):
        directories_to_process = [directory for directory in os.listdir(".") if directory[:4] == "exp_"]

        for group_session in tqdm(sorted(directories_to_process), unit="directories"):
            info(f"Processing group session {group_session}")

            if not is_directory_with_unified_xdf_files(group_session):
                process_directory_v1(group_session, out_dir)
            else:
                process_directory_v2(group_session, out_dir)


def process_directory_v1(group_session: str, out_dir: str):
    return process_directory(group_session, out_dir, lambda g, s: f"{g}/{s}/audio")


def process_directory_v2(group_session: str, out_dir: str):
    return process_directory(group_session, out_dir, lambda g, s: f"{g}/{s}/audio/block_2")


def process_directory(group_session: str, out_dir: str, audio_dir_fn: Callable):
    for station in ["lion", "tiger", "leopard"]:
        audio_dir = audio_dir_fn(group_session, station)
        if not os.path.exists(audio_dir):
            error(f"Audio folder does not exist for station {station} in group session {group_session}.")
            continue

        for audio_file in os.listdir(audio_dir):
            audio = PCMAudio(filepath=f"{audio_dir}/{audio_file}")

            os.makedirs(f"{out_dir}/{audio_dir}", exist_ok=True)
            df = audio.extract_vocalic_features()

            vocalic_filename = audio_file[:audio_file.rfind(".")] + "csv"
            df.to_csv(f"{out_dir}/{audio_dir}/{vocalic_filename}")


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

    logging.basicConfig(
        level=logging.INFO,
        handlers=(
            logging.FileHandler(
                filename=f"{LOG_DIR}/extract_vocalic_features.log",
                mode="w",
            ),
            logging.StreamHandler(stream=sys.stderr),
        ),
    )

    extract_vocalic_features(args.experiments_dir, args.out_dir)
