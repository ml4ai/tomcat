#!/usr/bin/env python

import logging
import sys
from logging import info

from config import USER
from entity.signal.fnirs import FNIRSRaw
from process_raw_signals import create_indices
from process_raw_signals import insert_raw_unlabeled_data
from process_raw_signals import label_data
from process_raw_signals import remove_invalid_data

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/build_fnirs_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def get_channel_names_from_xdf_stream(stream):
    return [channel["custom_name"][0].lower().replace("-", "_") + channel["type"][0][-4:].lower() for channel in
                stream["info"]["desc"][0]["channels"][0]["channel"][41:]]


def get_station_from_xdf_stream(group_session, stream):
    return stream["info"]["name"][0].split("_")[0]


def process_fnirs_raw_data(database_engine, override):
    info("Processing FNIRSRaw data.")
    insert_raw_unlabeled_data(database_engine, override, FNIRSRaw, "fnirs", "NIRS", get_channel_names_from_xdf_stream,
                              get_station_from_xdf_stream)
    create_indices(database_engine, not override, FNIRSRaw, "fnirs")
    label_data(database_engine, override, FNIRSRaw, "fnirs")
    remove_invalid_data(database_engine, FNIRSRaw, "fnirs")


def recreate_fnirs_raw_tables(database_engine):
    FNIRSRaw.__table__.drop(database_engine, checkfirst=True)
    FNIRSRaw.__table__.create(database_engine, checkfirst=True)


