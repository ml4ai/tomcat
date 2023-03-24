#!/bin/bash

IMAGE_TIMESTAMP_SESSION_NAME="image-timestamp"

__tmux_start_image_timestamp() {
    local experiment_path=$1

    local monitoring_lion_face="$experiment_path/lion/face_images/"
    local monitoring_lion_screen="$experiment_path/lion/screenshots/"
    local monitoring_tiger_face="$experiment_path/tiger/face_images/"
    local monitoring_tiger_screen="$experiment_path/tiger/screenshots/"
    local monitoring_leopard_face="$experiment_path/leopard/face_images/"
    local monitoring_leopard_screen="$experiment_path/leopard/screenshots/"

    mkdir -p "$monitoring_lion_face" >/dev/null
    mkdir -p "$monitoring_lion_screen" >/dev/null
    mkdir -p "$monitoring_tiger_face" >/dev/null
    mkdir -p "$monitoring_tiger_screen" >/dev/null
    mkdir -p "$monitoring_leopard_face" >/dev/null
    mkdir -p "$monitoring_leopard_screen" >/dev/null

    tmux new-session -d -s $IMAGE_TIMESTAMP_SESSION_NAME -n "lion-face" "~/git/ML4AI/tomcat/human_experiments/lab_software/tomcat-images-timestamp/build/fileChecker --out 1 --path $monitoring_lion_face"
    tmux new-window -t $IMAGE_TIMESTAMP_SESSION_NAME:1 -n "lion-screen" "~/git/ML4AI/tomcat/human_experiments/lab_software/tomcat-images-timestamp/build/fileChecker --out 1 --path $monitoring_lion_screen"
    tmux new-window -t $IMAGE_TIMESTAMP_SESSION_NAME:2 -n "tiger-face" "~/git/ML4AI/tomcat/human_experiments/lab_software/tomcat-images-timestamp/build/fileChecker --out 1 --path $monitoring_tiger_face"
    tmux new-window -t $IMAGE_TIMESTAMP_SESSION_NAME:3 -n "tiger-screen" "~/git/ML4AI/tomcat/human_experiments/lab_software/tomcat-images-timestamp/build/fileChecker --out 1 --path $monitoring_tiger_screen"
    tmux new-window -t $IMAGE_TIMESTAMP_SESSION_NAME:4 -n "leopard-face" "~/git/ML4AI/tomcat/human_experiments/lab_software/tomcat-images-timestamp/build/fileChecker --out 1 --path $monitoring_leopard_face"
    tmux new-window -t $IMAGE_TIMESTAMP_SESSION_NAME:5 -n "leopard-screen" "~/git/ML4AI/tomcat/human_experiments/lab_software/tomcat-images-timestamp/build/fileChecker --out 1 --path $monitoring_leopard_screen"
}

__tmux_start_image_timestamp $1
