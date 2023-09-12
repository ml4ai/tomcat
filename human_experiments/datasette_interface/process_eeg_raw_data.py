#!/usr/bin/env python

import logging
import sys
from logging import info

from config import USER
from entity.signal.eeg import EEGRaw
from entity.base.eeg_device import EEGDevice
from process_raw_signals import create_indices
from process_raw_signals import insert_raw_unlabeled_data
from process_raw_signals import label_data
from process_raw_signals import remove_invalid_data
from sqlalchemy.orm import Session
from functools import partial

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


def get_station_from_xdf_stream(group_session, stream, device_id_to_station_map):
    device_id = stream["info"]["name"][0].split("-")[1].replace("_actiCHamp", "")
    return device_id_to_station_map[group_session][device_id]


def process_eeg_raw_data(database_engine, override):
    info("Processing EEGRaw data.")

    device_id_to_station_map = {}
    with Session(database_engine) as database_session:
        for group_session, station_id, device_id in database_session.query(EEGDevice).all():
            if device_id:
                device_id_to_station_map[group_session] = {device_id: station_id}
    print(device_id_to_station_map)

    insert_raw_unlabeled_data(database_engine, override, EEGRaw, "eeg", "EEG", get_channel_names_from_xdf_stream,
                              partial(get_station_from_xdf_stream, device_id_to_station_map=device_id_to_station_map))
    create_indices(database_engine, not override, EEGRaw, "eeg")
    label_data(database_engine, override, EEGRaw, "eeg")
    remove_invalid_data(database_engine, EEGRaw, "eeg")


def recreate_eeg_raw_tables(database_engine):
    EEGRaw.__table__.drop(database_engine, checkfirst=True)
    EEGRaw.__table__.create(database_engine, checkfirst=True)

