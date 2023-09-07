#!/usr/bin/env python

import logging
import sys
from logging import info

from config import USER
from entity.signal.eeg import EEGRaw
from process_raw_signals import create_indices
from process_raw_signals import insert_raw_unlabeled_data
from process_raw_signals import label_data
from process_raw_signals import remove_invalid_data

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/build_eeg_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def get_channel_names_from_xdf_stream(stream):
    return [channel["label"][0].lower() for channel in stream["info"]["desc"][0]["channels"][0]["channel"]]


def get_station_from_xdf_stream(stream):
    return stream["info"]["name"][0].split("-")[1].replace("_actiCHamp", "")


def process_eeg_raw_data(database_engine, override):
    info("Processing EEGRaw data.")
    insert_raw_unlabeled_data(database_engine, override, EEGRaw, "eeg", "EEG", get_channel_names_from_xdf_stream,
                              get_station_from_xdf_stream)
    create_indices(database_engine, not override, EEGRaw, "eeg")
    label_data(database_engine, override, EEGRaw, "eeg")
    remove_invalid_data(database_engine, EEGRaw, "eeg")


def recreate_eeg_raw_tables(database_engine):
    EEGRaw.__table__.drop(database_engine, checkfirst=True)
    EEGRaw.__table__.create(database_engine, checkfirst=True)

