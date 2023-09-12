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


def transcribe_utterances(experiments_dir: str, out_dir: str):
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
    transcriber = Whisper()

    for station in ["lion", "tiger", "leopard"]:
        audio_dir = audio_dir_fn(experiment_dir, station)
        annotation_dir = f"{audio_dir}/annotations"
        if not os.path.exists(annotation_dir):
            error(
                f"Annotation folder does not exist for station {station} in group session {os.path.basename(experiment_dir)}.")
            continue

        for annotation_file in os.listdir(annotation_dir):
            if annotation_file[annotation_file.rfind("."):].lower() != ".textgrid":
                continue

            annotation = PraatAnnotation(filepath=f"{annotation_dir}/{annotation_file}")

            audio_file = annotation_file[:annotation_file.rfind(".")] + ".wav"
            audio = PCMAudio(filepath=f"{audio_dir}/{audio_file}")

            audio.transcribe_annotated_utterances(transcriber, annotation)

            # Save new annotation with transcriptions
            sub_dir = audio_dir[audio_dir.find("exp_"):] + "/transcriptions"
            os.makedirs(f"{out_dir}/{sub_dir}", exist_ok=True)

            transcriptions_filename = audio_file[:audio_file.rfind(".")]
            annotation.save(f"{out_dir}/{sub_dir}/{transcriptions_filename}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parses a collection of experiments, finds audio and the annotation files, extracts transcriptions"
                    " for the annotated utterances."
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
                filename=f"{args.log_dir}/transcribe_utterances.log",
                mode="w",
            ),
            logging.StreamHandler(stream=sys.stderr),
        ),
    )

    transcribe_utterances(args.experiments_dir, args.out_dir)
