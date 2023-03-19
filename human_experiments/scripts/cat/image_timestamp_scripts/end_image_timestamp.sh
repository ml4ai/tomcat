#!/bin/bash

IMAGE_TIMESTAMP_SESSION_NAME="image-timestamp"

__tmux_end_image_timestamp() {
    tmux kill-session -t $IMAGE_TIMESTAMP_SESSION_NAME
}

__tmux_end_image_timestamp
