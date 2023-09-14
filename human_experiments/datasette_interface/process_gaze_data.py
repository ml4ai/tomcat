#!/usr/bin/env python

import logging
import sys
from logging import info

from config import USER
from entity.signal.gaze import GAZERaw
from process_raw_signals import create_indices
from process_raw_signals import insert_raw_unlabeled_data
from process_raw_signals import label_data
from process_raw_signals import remove_invalid_data

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/build_gaze_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def get_channel_names_from_xdf_stream(stream):
    return [channel["label"][0].lower() for channel in stream["info"]["desc"][0]["channels"][0]["channel"]]


def get_station_from_xdf_stream(group_session, stream):
    return stream["info"]["hostname"][0]


def process_gaze_raw_data(database_engine, override):
    info("Processing GazeRaw data.")
    insert_raw_unlabeled_data(database_engine, override, GAZERaw, "Gaze", "Gaze", get_channel_names_from_xdf_stream,
                              get_station_from_xdf_stream)
    create_indices(database_engine, not override, GAZERaw, "Gaze")
    label_data(database_engine, override, GAZERaw, "Gaze")
    remove_invalid_data(database_engine, GAZERaw, "Gaze")


def recreate_gaze_raw_tables(database_engine):
    GAZERaw.__table__.drop(database_engine, checkfirst=True)
    GAZERaw.__table__.create(database_engine, checkfirst=True)
