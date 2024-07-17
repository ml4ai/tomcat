import argparse
import logging
import os
import sys
from logging import error, info
from typing import Callable

from tqdm import tqdm

from datasette_interface.model.audio.praat_annotation import PraatAnnotation
from datasette_interface.model.audio.sentence_labeler import (
    SentenceLabeler,
    ToMCATDialogAgent,
)
from datasette_interface.common.config import settings, LOG_DIR
from datasette_interface.common.utils import is_directory_with_unified_xdf_files


def label_transcriptions(out_dir: str, labeler: SentenceLabeler, override: bool):
    info("Processing directories...")

    directories_to_process = [
        directory
        for directory in os.listdir(settings.experiment_root_dir)
        if os.path.basename(directory)[:4] == "exp_"
    ]

    for group_session in tqdm(sorted(directories_to_process), unit="directories"):
        info(f"Processing group session {group_session}")

        experiment_dir = f"{settings.experiment_root_dir}/{group_session}"

        if not is_directory_with_unified_xdf_files(group_session):
            process_directory_v1(experiment_dir, out_dir, labeler, override)
        else:
            process_directory_v2(experiment_dir, out_dir, labeler, override)


def process_directory_v1(
    experiment_dir: str, out_dir: str, labeler: SentenceLabeler, override: bool
):
    return process_directory(
        experiment_dir, out_dir, lambda g, s: f"{g}/{s}/audio", labeler, override
    )


def process_directory_v2(
    experiment_dir: str, out_dir: str, labeler: SentenceLabeler, override: bool
):
    return process_directory(
        experiment_dir,
        out_dir,
        lambda g, s: f"{g}/{s}/audio/block_2",
        labeler,
        override,
    )


def process_directory(
    experiment_dir: str,
    out_dir: str,
    audio_dir_fn: Callable,
    labeler: SentenceLabeler,
    override: bool,
):
    for station in ["lion", "tiger", "leopard"]:
        audio_dir = audio_dir_fn(experiment_dir, station)
        transcriptions_annotation_dir = f"{audio_dir}/transcriptions"
        if not os.path.exists(transcriptions_annotation_dir):
            error(
                f"Transcripts do not exist for station {station} in group session "
                f"{os.path.basename(experiment_dir)}."
            )
            continue

        for annotation_file in os.listdir(transcriptions_annotation_dir):
            if annotation_file[annotation_file.rfind(".") :].lower() != ".textgrid":
                continue

            sub_dir = audio_dir[audio_dir.find("exp_") :] + "/dialog_labels"
            labels_filepath = f"{out_dir}/{sub_dir}/{annotation_file}"

            if os.path.exists(labels_filepath) and not override:
                info(
                    f"Skipping file {annotation_file}. Label annotations already found "
                    f"in {out_dir}."
                )
                continue

            annotation = PraatAnnotation(
                filepath=f"{transcriptions_annotation_dir}/{annotation_file}"
            )
            labeler.annotate_labels(annotation)

            # Save new annotation with labels
            os.makedirs(f"{out_dir}/{sub_dir}", exist_ok=True)
            annotation.save(labels_filepath)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parses a collection of experiments, finds audio and the annotation files, "
        "extracts transcriptions for the annotated utterances."
    )

    parser.add_argument(
        "--out_dir",
        type=str,
        required=False,
        help="Directory where experiment folder structure containing semantic labels must be "
        "saved.",
    )
    parser.add_argument(
        "--api_host",
        type=str,
        required=False,
        default="localhost",
        help="Host of the REST API for the dialog agent.",
    )
    parser.add_argument(
        "--api_port",
        type=int,
        required=False,
        default=8080,
        help="Ports of the REST API for the dialog agent.",
    )
    parser.add_argument(
        "--override",
        action="store_true",
        help="Do not reprocess data already processed.",
    )

    args = parser.parse_args()

    os.makedirs(LOG_DIR, exist_ok=True)
    logging.basicConfig(
        level=logging.INFO,
        handlers=(
            logging.FileHandler(
                filename=f"{LOG_DIR}/label_transcriptions.log",
                mode="w",
            ),
            logging.StreamHandler(stream=sys.stderr),
        ),
    )

    dialog_agent = ToMCATDialogAgent(args.api_host, args.api_port)
    label_transcriptions(args.out_dir, dialog_agent, args.override)
