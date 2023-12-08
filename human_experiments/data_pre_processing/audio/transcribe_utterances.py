import argparse
import logging
import os
import sys
from logging import error, info
from typing import Callable

from tqdm import tqdm

from audio.entity.pcm_audio import PCMAudio
from audio.entity.praat_annotation import PraatAnnotation
from audio.entity.transcriber import Whisper
from common.config import EXP_DIR, LOG_DIR, OUT_DIR
from utils import is_directory_with_unified_xdf_files


def transcribe_utterances(
    experiments_dir: str, annotations_dir_name: str, out_dir: str, override: bool
):
    info("Processing directories...")

    directories_to_process = [
        directory
        for directory in os.listdir(experiments_dir)
        if os.path.basename(directory)[:4] == "exp_"
    ]

    for group_session in tqdm(sorted(directories_to_process), unit="directories"):
        info(f"Processing group session {group_session}")

        experiment_dir = f"{experiments_dir}/{group_session}"

        if not is_directory_with_unified_xdf_files(group_session):
            process_directory_v1(
                experiment_dir, annotations_dir_name, out_dir, override
            )
        else:
            process_directory_v2(
                experiment_dir, annotations_dir_name, out_dir, override
            )


def process_directory_v1(
    experiment_dir: str, annotations_dir_name: str, out_dir: str, override: bool
):
    return process_directory(
        experiment_dir,
        annotations_dir_name,
        out_dir,
        lambda g, s: f"{g}/{s}/audio",
        override,
    )


def process_directory_v2(
    experiment_dir: str, annotations_dir_name: str, out_dir: str, override: bool
):
    return process_directory(
        experiment_dir,
        annotations_dir_name,
        out_dir,
        lambda g, s: f"{g}/{s}/audio/block_2",
        override,
    )


def process_directory(
    experiment_dir: str,
    annotations_dir_name: str,
    out_dir: str,
    audio_dir_fn: Callable,
    override: bool,
):
    transcriber = Whisper()

    for station in tqdm(
        ["lion", "tiger", "leopard"], position=1, leave=False, total=3, desc="Station"
    ):
        audio_dir = audio_dir_fn(experiment_dir, station)
        annotation_dir = f"{audio_dir}/{annotations_dir_name}"
        if not os.path.exists(annotation_dir):
            error(
                f"Annotation folder does not exist for station {station} in group session {os.path.basename(experiment_dir)}."
            )
            continue

        for annotation_file in os.listdir(annotation_dir):
            if annotation_file[annotation_file.rfind(".") :].lower() != ".textgrid":
                continue

            sub_dir = audio_dir[audio_dir.find("exp_") :] + "/transcriptions"
            transcripts_filepath = f"{out_dir}/{sub_dir}/{annotation_file}"

            if os.path.exists(transcripts_filepath) and not override:
                info(
                    f"Skipping file {annotation_file}. Transcript annotations already found in {out_dir}."
                )
                continue

            annotation = PraatAnnotation(filepath=f"{annotation_dir}/{annotation_file}")

            audio_file = annotation_file[: annotation_file.rfind(".")] + ".wav"
            audio = PCMAudio(filepath=f"{audio_dir}/{audio_file}")

            audio.transcribe_annotated_utterances(transcriber, annotation)

            # Save new annotation with transcriptions
            os.makedirs(f"{out_dir}/{sub_dir}", exist_ok=True)
            annotation.save(transcripts_filepath)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parses a collection of experiments, finds audio and the annotation files, extracts transcriptions"
        " for the annotated utterances."
    )

    parser.add_argument(
        "--experiments_dir",
        type=str,
        required=False,
        default=EXP_DIR,
        help="Directory containing experiment folders.",
    )
    parser.add_argument(
        "--annotations_dir_name",
        type=str,
        required=False,
        default="annotations",
        help="Name of the directory containing annotations inside the audio folder.",
    )
    parser.add_argument(
        "--out_dir",
        type=str,
        required=False,
        default=OUT_DIR,
        help="Directory where experiment folder structure containing transcriptions must be saved.",
    )
    parser.add_argument(
        "--log_dir",
        type=str,
        required=False,
        default=LOG_DIR,
        help="Directory where log files must be saved.",
    )
    parser.add_argument(
        "--override",
        action="store_true",
        help="Do not reprocess data already processed.",
    )

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

    transcribe_utterances(
        args.experiments_dir, args.annotations_dir_name, args.out_dir, args.override
    )
