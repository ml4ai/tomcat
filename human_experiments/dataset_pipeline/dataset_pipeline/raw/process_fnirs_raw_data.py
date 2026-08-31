#!/usr/bin/env python

from logging import info

from dataset_pipeline.common.config import LOG_DIR, configure_logging
from dataset_pipeline.database.entity.signal.fnirs import FNIRSRaw
from dataset_pipeline.raw.common.process_raw_signals import (
    insert_raw_unlabeled_data,
    label_data,
)


def get_channel_names_from_xdf_stream(stream):
    hb_channels = [
        channel["custom_name"][0].lower().replace("-", "_")
        + channel["type"][0][-4:].lower()
        for channel in stream["info"]["desc"][0]["channels"][0]["channel"][41:]
    ]
    raw_channels = [
        channel["custom_name"][0].lower().replace("-", "_")
        + channel["type"][0][-4:].lower()
        + str(int(float(channel["wavelength"][0])))
        for channel in stream["info"]["desc"][0]["channels"][0]["channel"][1:41]
    ]
    return raw_channels + hb_channels


def get_station_from_xdf_stream(group_session, stream):
    return stream["info"]["name"][0].split("_")[0]


def process_fnirs_raw_data():
    configure_logging(f"{LOG_DIR}/build_fnirs_table.log")
    info("Processing FNIRSRaw data.")
    insert_raw_unlabeled_data(
        FNIRSRaw,
        "fnirs",
        "NIRS",
        get_channel_names_from_xdf_stream,
        get_station_from_xdf_stream,
        lambda x: x[1:],
    )
    label_data(FNIRSRaw, "fnirs")
