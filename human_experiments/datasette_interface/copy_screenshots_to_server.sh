#!/usr/bin/env bash
# Script to copy screenshots to the ivilab webserver. This is meant to be run
# on an IVILab machine on which /data is mounted.

time rsync \
    -adrPm \
    --no-inc-recursive \
    --copy-links \
    --info=progress2 \
    --include="*/" \
    --include="*/*/Screenshots/*.png" \
    --include="*/*/screenshots/*.png" \
    --include="*/*/screenshots/block_*/*.png" \
    --exclude="*" \
    /data/tomcat/raw/LangLab/experiments/study_3_pilot/group ivilab:/tomcat/data

