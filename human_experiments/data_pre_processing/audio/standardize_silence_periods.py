import argparse
import logging
import os
import sys
from logging import info, error
from typing import Callable

from tqdm import tqdm

from audio.entity.pcm_audio import PCMAudio
from audio.entity.praat_annotation import PraatAnnotation
from common.config import EXP_DIR, OUT_DIR, LOG_DIR
from utils import cd, is_directory_with_unified_xdf_files

from audio.entity.transcriber import Whisper


def standardize_silence_periods(experiments_dir: str, out_dir: str, silence_threshold: int, override: bool):
    info("Processing directories...")

    directories_to_process = [directory for directory in os.listdir(experiments_dir) if
                              os.path.basename(directory)[:4] == "exp_"]

    for group_session in tqdm(sorted(directories_to_process), unit="directories"):
        info(f"Processing group session {group_session}")

        experiment_dir = f"{experiments_dir}/{group_session}"

        if not is_directory_with_unified_xdf_files(group_session):
            process_directory_v1(experiment_dir, out_dir, silence_threshold, override)
        else:
            process_directory_v2(experiment_dir, out_dir, silence_threshold, override)


def process_directory_v1(experiment_dir: str, out_dir: str, silence_threshold: int, override: bool):
    return process_directory(experiment_dir, out_dir, lambda g, s: f"{g}/{s}/audio", silence_threshold, override)


def process_directory_v2(experiment_dir: str, out_dir: str, silence_threshold: int, override: bool):
    return process_directory(experiment_dir, out_dir, lambda g, s: f"{g}/{s}/audio/block_2", silence_threshold, override)


def process_directory(experiment_dir: str, out_dir: str, audio_dir_fn: Callable, silence_threshold: int, override: bool):
    for station in ["lion", "tiger", "leopard"]:
        audio_dir = audio_dir_fn(experiment_dir, station)
        annotation_dir = f"{audio_dir}/annotations"
        if not os.path.exists(annotation_dir):
            error(
                f"Annotation folder does not exist for station {station} in group session {os.path.basename(experiment_dir)}.")
            continue

        for annotation_file in os.listdir(annotation_dir):
            if annotation_file[annotation_file.rfind("."):].lower() != ".textgrid" or annotation_file[0] == ".":
                continue

            # Add the threshold to the directory name to be created to store the standardized annotations.
            sub_dir = audio_dir[audio_dir.find("exp_"):] + f"/standardized_silences_{silence_threshold}s"
            standardized_annotation_filepath = f"{out_dir}/{sub_dir}/{annotation_file}"

            if os.path.exists(standardized_annotation_filepath) and not override:
                info(f"Skipping file {annotation_file}. Standardized annotations already found in {out_dir}.")
                continue

            annotation = PraatAnnotation(filepath=f"{annotation_dir}/{annotation_file}")
            annotation.standardize_silences(silence_threshold)

            # Save standardized annotation
            os.makedirs(f"{out_dir}/{sub_dir}", exist_ok=True)
            annotation.save(standardized_annotation_filepath)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parses a collection of experiments, finds the annotation files and merge subsequent sound "
                    "periods if they are less than X seconds apart, where X is an argument of this script."
    )

    parser.add_argument("--experiments_dir", type=str, required=False, default=EXP_DIR,
                        help="Directory containing experiment folders.")
    parser.add_argument("--out_dir", type=str, required=False, default=OUT_DIR,
                        help="Directory where experiment folder structure containing standardized silence annotations must be saved.")
    parser.add_argument("--silence_threshold", type=float, required=False, default=1,
                        help="Minimum duration of a silence period. If subsequent utterances are less than this "
                             "duration apart, they will be merged into a single interval.")
    parser.add_argument("--log_dir", type=str, required=False, default=LOG_DIR,
                        help="Directory where log files must be saved.")
    parser.add_argument("--override", action='store_true', help="Do not reprocess data already processed.")

    args = parser.parse_args()

    os.makedirs(args.log_dir, exist_ok=True)
    logging.basicConfig(
        level=logging.INFO,
        handlers=(
            logging.FileHandler(
                filename=f"{args.log_dir}/standardize_silence_periods.log",
                mode="w",
            ),
            logging.StreamHandler(stream=sys.stderr),
        ),
    )

    standardize_silence_periods(args.experiments_dir, args.out_dir, args.silence_threshold, args.override)
