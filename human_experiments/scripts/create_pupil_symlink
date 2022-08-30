#!/bin/bash

set -u

source configuration_helpers

if [[ -z ${1+x} ]]; then
  TARGET_DIR=""
else
  TARGET_DIR=$1
fi

if [[ -z ${2+x} ]]; then
  PADDING=""
else
  PADDING=$2
fi

# Local directory where pupil data is saved in the client machines
PUPIL_RECORDER_LOCAL_DIR="$HOME/recordings/$(date +%Y_%m_%d)"

# Remove any previous link that might exist
if [[ -d "$PUPIL_RECORDER_LOCAL_DIR" ]]; then
  unlink "$PUPIL_RECORDER_LOCAL_DIR"
fi

if ln -s "$TARGET_DIR" "$PUPIL_RECORDER_LOCAL_DIR"; then
  echo -e "${PADDING}${GREEN}Symbolic link ${EMPH}$TARGET_DIR -> $PUPIL_RECORDER_LOCAL_DIR${GREEN} successfully created.${NC}"
  exit 0
else
  echo -e "${PADDING}${RED}Failed to create symbolic link ${EMPH}$TARGET_DIR -> $PUPIL_RECORDER_LOCAL_DIR${RED}.${NC}"
  exit 1
fi
