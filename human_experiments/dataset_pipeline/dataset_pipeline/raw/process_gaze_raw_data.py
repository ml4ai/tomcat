#!/usr/bin/env python

from logging import info

from dataset_pipeline.common.config import LOG_DIR, configure_logging
from dataset_pipeline.database.entity.signal.gaze import GAZERaw
from dataset_pipeline.raw.common.process_raw_signals import (
    insert_raw_unlabeled_data,
    label_data,
)


def get_channel_names_from_xdf_stream(stream):
    return [
        channel["label"][0].lower()
        for channel in stream["info"]["desc"][0]["channels"][0]["channel"]
    ]


def get_station_from_xdf_stream(group_session, stream):
    return stream["info"]["hostname"][0]


def process_gaze_raw_data():
    configure_logging(f"{LOG_DIR}/build_gaze_table.log")
    info("Processing GazeRaw data.")
    insert_raw_unlabeled_data(
        GAZERaw,
        "Gaze",
        "Gaze",
        get_channel_names_from_xdf_stream,
        get_station_from_xdf_stream,
    )
    label_data(GAZERaw, "Gaze")
