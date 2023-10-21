#!/usr/bin/env python
"""Script to process screen capture data"""
import json
import logging
import os
import sys
from glob import glob
from logging import info, error, debug
import time
from dateutil import parser

import pyxdf
from sqlalchemy.orm import Session
from tqdm import tqdm

from config import USER
from entity.base.base import Base
from entity.signal.screen_capture import ScreenCapture
from utils import (
    cd,
    should_ignore_directory,
    convert_iso8601_timestamp_to_unix,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
)
from process_raw_signals import create_indices, label_data
from process_image_data import insert_raw_unlabeled_data

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/build_screen_capture_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def process_screen_capture_data(database_engine, override):
    info(
        """
        Processing screen capture data. For the group sessions predating 2022-11-07
        the file creation date was not saved separately and the image file names do not
        contain the creation timestamps. In those cases, the last modification timestamp from
        the file metadata is used instead which is just a few milliseconds delayed.
        """
    )

    info("Processing directories...")

    insert_raw_unlabeled_data(database_engine, override, ScreenCapture, "screen capture", "Screen")
    create_indices(database_engine, not override, ScreenCapture, "screen_capture")
    # Here we can pass any physio modality. This is used just to retrieve distinct rows of the data
    # validity table. We pass eeg but it could have been fnirs or gaze.
    label_data(database_engine, override, ScreenCapture, "eeg")


def recreate_screen_capture_tables(database_engine):
    ScreenCapture.__table__.drop(database_engine, checkfirst=True)
    ScreenCapture.__table__.create(database_engine, checkfirst=True)
