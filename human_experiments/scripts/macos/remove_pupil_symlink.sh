#!/bin/bash

set -u

source ../configuration_helpers

# This script must be executed in the client side.

if [[ -z ${1+x} ]]; then
  PADDING=""
else
  PADDING=$1
fi

# Local directory where pupil data is saved in the client machines
PUPIL_RECORDER_LOCAL_DIR="$HOME/recordings/$(date +%Y_%m_%d)"

# Remove a previous link that might exist
if [[ -d "$PUPIL_RECORDER_LOCAL_DIR" ]]; then
  if ! unlink "$PUPIL_RECORDER_LOCAL_DIR"; then
    echo -e "${PADDING}${RED}Failed to remove the symbolic link ${EMPH}$PUPIL_RECORDER_LOCAL_DIR${RED}.${NC}"
  fi
fi
