#!/usr/bin/env python

import logging
import sys
from logging import info

from datasette_interface.common.config import LOG_DIR
from datasette_interface.database.entity.signal.fnirs import FNIRSRaw
from datasette_interface.raw.common.process_raw_signals import (
    insert_raw_unlabeled_data, label_data)

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(filename=f"{LOG_DIR}/build_fnirs_table.log", mode="w"),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def get_channel_names_from_xdf_stream(stream):
    hb_channels = [
        channel["custom_name"][0].lower().replace("-", "_")
        + channel["type"][0][-4:].lower()
        for channel in stream["info"]["desc"][0]["channels"][0]["channel"][41:]
    ]
    raw_channels = [
        channel["custom_name"][0].lower().replace("-", "_")
        + channel["type"][0][-4:].lower() + str(int_float(channel["wavelength"][0]))
        for channel in stream["info"]["desc"][0]["channels"][0]["channel"][1:41]
    ]
    return hb_channels + raw_channels


def get_station_from_xdf_stream(group_session, stream):
    return stream["info"]["name"][0].split("_")[0]


def process_fnirs_raw_data():
    info("Processing FNIRSRaw data.")
    insert_raw_unlabeled_data(
        FNIRSRaw,
        "fnirs",
        "NIRS",
        get_channel_names_from_xdf_stream,
        get_station_from_xdf_stream,
        lambda x: x[41:],
    )
    label_data(FNIRSRaw, "fnirs")
