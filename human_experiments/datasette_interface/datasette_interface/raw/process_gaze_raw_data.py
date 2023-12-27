#!/usr/bin/env python

import logging
import sys
from logging import info

import numpy as np

from datasette_interface.common.config import LOG_DIR
from datasette_interface.database.entity.signal.gaze import (GazeEye0Raw,
                                                             GazeEye1Raw,
                                                             GazeMixin)
from datasette_interface.raw.common.process_raw_signals import (
    insert_raw_unlabeled_data, label_data)

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(filename=f"{LOG_DIR}/build_gaze_table.log", mode="w"),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def get_channel_names_from_xdf_stream(stream):
    return [
        channel["label"][0].lower()
        for channel in stream["info"]["desc"][0]["channels"][0]["channel"]
    ]


def get_station_from_xdf_stream(group_session, stream):
    return stream["info"]["hostname"][0]


def gaze_factory(
    group_session_id: str,
    station_id: str,
    participant_id: int,
    id: int,
    task_id: str,
    timestamp_unix: str,
    timestamp_iso8601: str,
    confidence: float,
    norm_pos_x: float,
    norm_pos_y: float,
    gaze_point_3d_x: float,
    gaze_point_3d_y: float,
    gaze_point_3d_z: float,
    eye_center0_3d_x: float,
    eye_center0_3d_y: float,
    eye_center0_3d_z: float,
    eye_center1_3d_x: float,
    eye_center1_3d_y: float,
    eye_center1_3d_z: float,
    gaze_normal0_x: float,
    gaze_normal0_y: float,
    gaze_normal0_z: float,
    gaze_normal1_x: float,
    gaze_normal1_y: float,
    gaze_normal1_z: float,
    diameter0_2d: float,
    diameter1_2d: float,
    diameter0_3d: float,
    diameter1_3d: float,
) -> GazeMixin:
    if np.isnan(eye_center0_3d_x):
        return GazeEye1Raw(
            group_session_id=group_session_id,
            station_id=station_id,
            participant_id=participant_id,
            id=id,
            task_id=task_id,
            timestamp_unix=timestamp_unix,
            timestamp_iso8601=timestamp_iso8601,
            confidence=confidence,
            norm_pos_x=norm_pos_x,
            norm_pos_y=norm_pos_y,
            gaze_point_3d_x=gaze_point_3d_x,
            gaze_point_3d_y=gaze_point_3d_y,
            gaze_point_3d_z=gaze_point_3d_z,
            eye_center_3d_x=eye_center1_3d_x,
            eye_center_3d_y=eye_center1_3d_y,
            eye_center_3d_z=eye_center1_3d_z,
            gaze_normal_x=gaze_normal1_x,
            gaze_normal_y=gaze_normal1_y,
            gaze_normal_z=gaze_normal1_z,
            diameter_2d=diameter1_2d,
            diameter_3d=diameter1_3d,
        )
    else:
        return GazeEye0Raw(
            group_session_id=group_session_id,
            station_id=station_id,
            participant_id=participant_id,
            id=id,
            task_id=task_id,
            timestamp_unix=timestamp_unix,
            timestamp_iso8601=timestamp_iso8601,
            confidence=confidence,
            norm_pos_x=norm_pos_x,
            norm_pos_y=norm_pos_y,
            gaze_point_3d_x=gaze_point_3d_x,
            gaze_point_3d_y=gaze_point_3d_y,
            gaze_point_3d_z=gaze_point_3d_z,
            eye_center_3d_x=eye_center0_3d_x,
            eye_center_3d_y=eye_center0_3d_y,
            eye_center_3d_z=eye_center0_3d_z,
            gaze_normal_x=gaze_normal0_x,
            gaze_normal_y=gaze_normal0_y,
            gaze_normal_z=gaze_normal0_z,
            diameter_2d=diameter0_2d,
            diameter_3d=diameter0_3d,
        )


def process_gaze_raw_data():
    info("Processing GazeRaw data.")
    insert_raw_unlabeled_data(
        gaze_factory,
        "Gaze",
        "Gaze",
        get_channel_names_from_xdf_stream,
        get_station_from_xdf_stream,
    )
    label_data(GazeEye0Raw, "Gaze")
    label_data(GazeEye1Raw, "Gaze")
