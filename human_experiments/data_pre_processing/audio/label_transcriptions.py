import argparse
import logging
import os
import sys
from logging import info, error
from typing import Callable

from tqdm import tqdm

from audio.entity.praat_annotation import PraatAnnotation
from audio.entity.sentence_labeler import ToMCATDialogAgent, SentenceLabeler
from common.config import EXP_DIR, OUT_DIR, LOG_DIR
from utils import is_directory_with_unified_xdf_files


def label_transcriptions(experiments_dir: str, out_dir: str, labeler: SentenceLabeler):
    info("Processing directories...")

    directories_to_process = [directory for directory in os.listdir(experiments_dir) if
                              os.path.basename(directory)[:4] == "exp_"]

    for group_session in tqdm(sorted(directories_to_process), unit="directories"):
        info(f"Processing group session {group_session}")

        experiment_dir = f"{experiments_dir}/{group_session}"

        if not is_directory_with_unified_xdf_files(experiment_dir):
            process_directory_v1(experiment_dir, out_dir, labeler)
        else:
            process_directory_v2(experiment_dir, out_dir, labeler)


def process_directory_v1(experiment_dir: str, out_dir: str, labeler: SentenceLabeler):
    return process_directory(experiment_dir, out_dir, lambda g, s: f"{g}/{s}/audio", labeler)


def process_directory_v2(experiment_dir: str, out_dir: str, labeler: SentenceLabeler):
    return process_directory(experiment_dir, out_dir, lambda g, s: f"{g}/{s}/audio/block_2", labeler)


def process_directory(experiment_dir: str, out_dir: str, audio_dir_fn: Callable, labeler: SentenceLabeler):
    for station in ["lion", "tiger", "leopard"]:
        audio_dir = audio_dir_fn(experiment_dir, station)
        transcriptions_annotation_dir = f"{audio_dir}/transcriptions"
        if not os.path.exists(transcriptions_annotation_dir):
            error(
                f"Transcripts do not exist for station {station} in group session {os.path.basename(experiment_dir)}.")
            continue

        for annotation_file in os.listdir(transcriptions_annotation_dir):
            if annotation_file[annotation_file.rfind("."):].lower() != ".textgrid":
                continue

            annotation = PraatAnnotation(filepath=f"{transcriptions_annotation_dir}/{annotation_file}")
            labeler.annotate_labels(annotation)

            # Save new annotation with transcriptions
            sub_dir = audio_dir[audio_dir.find("exp_"):] + "/dialog_labels"
            os.makedirs(f"{out_dir}/{sub_dir}", exist_ok=True)

            labels_filename = annotation_file[:annotation_file.rfind(".")]
            annotation.save(f"{out_dir}/{sub_dir}/{labels_filename}")


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
    parser.add_argument("--api_host", type=str, required=False, default="localhost",
                        help="Host of the REST API for the dialog agent.")
    parser.add_argument("--api_port", type=int, required=False, default=8080,
                        help="Ports of the REST API for the dialog agent.")

    args = parser.parse_args()

    os.makedirs(args.log_dir, exist_ok=True)
    logging.basicConfig(
        level=logging.INFO,
        handlers=(
            logging.FileHandler(
                filename=f"{args.log_dir}/label_transcriptions.log",
                mode="w",
            ),
            logging.StreamHandler(stream=sys.stderr),
        ),
    )

    dialog_agent = ToMCATDialogAgent(args.api_host, args.api_port)
    label_transcriptions(args.experiments_dir, args.out_dir, dialog_agent)
