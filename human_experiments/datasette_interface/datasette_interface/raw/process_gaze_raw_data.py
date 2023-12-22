#!/usr/bin/env python

import logging
import sys
from logging import info

from datasette_interface.database.entity.signal import GAZERaw
from datasette_interface.raw.common.process_raw_signals import create_indices
from datasette_interface.raw.common.process_raw_signals import insert_raw_unlabeled_data
from datasette_interface.raw.common.process_raw_signals import label_data
from datasette_interface.database.config import get_db, engine
from datasette_interface.common.config import LOG_DIR, settings

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"{LOG_DIR}/build_gaze_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def get_channel_names_from_xdf_stream(stream):
    return ([channel["label"][0].lower() for channel in
             stream["info"]["desc"][0]["channels"][0]["channel"]])


def get_station_from_xdf_stream(group_session, stream):
    return stream["info"]["hostname"][0]


def process_gaze_raw_data():
    info("Processing GazeRaw data.")
    insert_raw_unlabeled_data(settings.drop_table, GAZERaw, "Gaze", "Gaze",
                              get_channel_names_from_xdf_stream,
                              get_station_from_xdf_stream)
    create_indices(not settings.drop_table, GAZERaw, "Gaze")
    label_data(settings.drop_table, GAZERaw, "Gaze")


def recreate_gaze_raw_tables():
    GAZERaw.__table__.drop(engine, checkfirst=True)
    GAZERaw.__table__.create(engine, checkfirst=True)
