#!/usr/bin/sh

# Script to install plugins into datasette container

docker pull datasetteproject/datasette:0.64.3

docker run datasetteproject/datasette \
    pip install \
        datasette-pretty-json \
        datasette-copyable \ # plugin for outputting tables in formats suitable for copy and paste
        datasette-render-image-tags # Turn any URLs ending in .jpg/.png/.gif into img tags with width 200

docker commit $(docker ps -lq) datasette-with-plugins
