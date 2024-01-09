#!/usr/bin/env python

import logging
import sys
from functools import partial
from logging import info

from datasette_interface.common.config import LOG_DIR
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.base.eeg_device import EEGDevice
from datasette_interface.database.entity.signal.eeg import EEGRaw
from datasette_interface.raw.common.process_raw_signals import (
    insert_raw_unlabeled_data, label_data)

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(filename=f"{LOG_DIR}/build_eeg_table.log", mode="w"),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def get_channel_names_from_xdf_stream(stream):
    return [
        channel["label"][0].lower()
        for channel in stream["info"]["desc"][0]["channels"][0]["channel"]
    ]


def get_station_from_xdf_stream(group_session, stream, device_id_to_station_map):
    device_id = stream["info"]["name"][0].split("-")[1].replace("_actiCHamp", "")
    return device_id_to_station_map[group_session][device_id]


def swap_channels_fn(signal):
    # There were some swaps between GSR and EKG channels in some experiments and stations. Here we
    # swap the channels so the signals are saved in the correct columns.
    swap_aux_channels = (signal["group_session_id"] == "exp_2022_09_30_10" and signal[
        "station_id"] == "lion")
    swap_aux_channels |= (
                signal["group_session_id"] == "exp_2022_10_04_09" and signal["station_id"] in [
            "lion", "tiger"])

    if swap_aux_channels:
        info("GSR and EKG were swapped in the experiment. We fix this by swapping aux_gsr with "
             "aux_ekg.")
        tmp = signal["aux_ekg"]
        signal["aux_ekg"] = signal["aux_gsr"]
        signal["aux_gsr"] = tmp


def process_eeg_raw_data():
    info("Processing EEGRaw data.")

    device_id_to_station_map = {}
    db = next(get_db())
    for eeg_device in db.query(EEGDevice).all():
        if eeg_device.device_id:
            if eeg_device.group_session_id not in device_id_to_station_map:
                device_id_to_station_map[eeg_device.group_session_id] = {
                    eeg_device.device_id: eeg_device.station_id
                }
            else:
                device_id_to_station_map[eeg_device.group_session_id][
                    eeg_device.device_id
                ] = eeg_device.station_id
    db.close()

    insert_raw_unlabeled_data(
        EEGRaw,
        "eeg",
        "EEG",
        get_channel_names_from_xdf_stream,
        partial(
            get_station_from_xdf_stream,
            device_id_to_station_map=device_id_to_station_map,
        ),
        lambda x: x * 1 - 6,
        swap_channels_fn
    )  # From micro-volt to volt
    label_data(EEGRaw, "eeg")
