#!/usr/bin/sh

# Script to install plugins into datasette container

docker pull datasetteproject/datasette:0.64.3

docker run datasetteproject/datasette \
    pip install \
        datasette-pretty-json


docker commit $(docker ps -lq) datasette-with-plugins
