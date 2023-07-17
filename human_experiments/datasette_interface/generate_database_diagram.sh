#!/usr/bin/env bash

# This script generates an up-to-date schema diagram for the ToMCAT database.
# It uses the SchemaCrawler tool. You can get it from here: https://github.com/schemacrawler/SchemaCrawler/releases
# To run the script, ensure that the directory containing the schemacrawler.sh
# script is in your PATH.

schemacrawler.sh \
    -c schema \
    -F png \
    -o static/db_diagram.png \
    --database /space/$USER/tomcat/tomcat.db \
    --info-level standard \
    --server sqlite \
    --portable-names \
    -g schemacrawler.config.properties
