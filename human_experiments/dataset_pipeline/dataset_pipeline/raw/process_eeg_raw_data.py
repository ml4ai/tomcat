#!/usr/bin/env python

from functools import partial
from logging import info

from dataset_pipeline.common.config import LOG_DIR, configure_logging
from dataset_pipeline.database.config import get_db
from dataset_pipeline.database.entity.base.eeg_device import EEGDevice
from dataset_pipeline.database.entity.signal.eeg import EEGRaw
from dataset_pipeline.raw.common.process_raw_signals import (
    insert_raw_unlabeled_data,
    label_data,
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
    swap_aux_channels = (
        signal["group_session_id"] == "exp_2022_09_30_10"
        and signal["station_id"] == "lion"
    )
    swap_aux_channels |= signal["group_session_id"] == "exp_2022_10_04_09" and signal[
        "station_id"
    ] in ["lion", "tiger"]

    if swap_aux_channels:
        tmp = signal["aux_ekg"]
        signal["aux_ekg"] = signal["aux_gsr"]
        signal["aux_gsr"] = tmp


def process_eeg_raw_data():
    configure_logging(f"{LOG_DIR}/build_eeg_table.log")
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
        lambda x: x,  # original signal in volt
        swap_channels_fn,
    )
    label_data(EEGRaw, "eeg")
