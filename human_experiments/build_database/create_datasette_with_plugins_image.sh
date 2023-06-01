#!/usr/bin/sh

# Script to install plugins into datasette container

docker run datasetteproject/datasette \
    pip install datasette-auth-passwords

docker commit $(docker ps -lq) datasette-with-plugins
