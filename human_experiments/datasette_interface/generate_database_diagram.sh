#!/usr/bin/env bash

schemacrawler.sh \
    -c schema \
    -F png \
    -o static/db_diagram.png \
    --database /space/$USER/tomcat/tomcat.db \
    --info-level standard \
    --server sqlite \
    --portable-names \
    -g schemacrawler.config.properties
