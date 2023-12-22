#!/usr/bin/env python

import logging
import sys
from logging import info

from datasette_interface.database.entity.signal.fnirs import FNIRSRaw
from datasette_interface.raw.common.process_raw_signals import create_indices
from datasette_interface.raw.common.process_raw_signals import insert_raw_unlabeled_data
from datasette_interface.raw.common.process_raw_signals import label_data
from datasette_interface.database.config import get_db, engine
from datasette_interface.common.config import LOG_DIR, settings

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"{LOG_DIR}/build_fnirs_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def get_channel_names_from_xdf_stream(stream):
    return ([channel["custom_name"][0].lower().replace("-", "_") +
             channel["type"][0][-4:].lower() for channel in
             stream["info"]["desc"][0]["channels"][0]["channel"][41:]])


def get_station_from_xdf_stream(group_session, stream):
    return stream["info"]["name"][0].split("_")[0]


def process_fnirs_raw_data():
    info("Processing FNIRSRaw data.")
    insert_raw_unlabeled_data(settings.drop_table, FNIRSRaw, "fnirs", "NIRS",
                              get_channel_names_from_xdf_stream,
                              get_station_from_xdf_stream, lambda x: x[41:])
    create_indices(not settings.drop_table, FNIRSRaw, "fnirs")
    label_data(settings.drop_table, FNIRSRaw, "fnirs")


def recreate_fnirs_raw_tables():
    FNIRSRaw.__table__.drop(engine, checkfirst=True)
    FNIRSRaw.__table__.create(engine, checkfirst=True)
