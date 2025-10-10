#!/usr/bin/env python
"""Script to process screen capture data"""

import logging
import sys
from logging import info

from datasette_interface.common.config import LOG_DIR
from datasette_interface.database.entity.signal.screen_capture import ScreenCapture
from datasette_interface.raw.common.process_image_data import insert_raw_unlabeled_data
from datasette_interface.raw.common.process_raw_signals import (
    label_data,
    remove_unlabeled_data,
)

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"{LOG_DIR}/build_screen_capture_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def process_screen_capture_data():
    info(
        """
        Processing screen capture data. For the group sessions predating 2022-11-07
        the file creation date was not saved separately and the image file names do not
        contain the creation timestamps. In those cases, the last modification timestamp from
        the file metadata is used instead which is just a few milliseconds delayed.
        """
    )

    info("Processing directories...")

    insert_raw_unlabeled_data(ScreenCapture, "screen capture", "Screen", "screenshots")
    # Here we can pass any physio modality. This is used just to retrieve distinct rows of the data
    # validity table. We pass eeg but it could have been fnirs or gaze.
    label_data(ScreenCapture, "eeg")
    remove_unlabeled_data(ScreenCapture)
