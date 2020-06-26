#!/bin/bash

DATA_FOLDER="$(cd "$(dirname "${BASH_SOURCE[0]}")/data" >/dev/null 2>&1 && pwd)"

# Creating folder to store the mission maps
mkdir -p "$DATA_FOLDER/maps"

# Creating folder to store the raw and formatted data
mkdir -p "$DATA_FOLDER/experiments/asist/raw/singleplayer"
mkdir -p "$DATA_FOLDER/experiments/asist/raw/sparky"
mkdir -p "$DATA_FOLDER/experiments/asist/raw/falcon"
mkdir -p "$DATA_FOLDER/experiments/asist/formatted/singleplayer"
mkdir -p "$DATA_FOLDER/experiments/asist/formatted/sparky"
mkdir -p "$DATA_FOLDER/experiments/asist/formatted/falcon"

# Creating folders to store data in a format compatible with the model implementation
mkdir -p "$DATA_FOLDER/evidence/asist/singleplayer"
mkdir -p "$DATA_FOLDER/evidence/asist/sparky"
mkdir -p "$DATA_FOLDER/evidence/asist/falcon"

