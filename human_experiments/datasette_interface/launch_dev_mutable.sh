#!/usr/bin/env bash
# Shell script for launching the Datasette instance in dev mode

set -euo pipefail
datasette \
    /space/${USER}/tomcat/tomcat.db \
    --port ${TOMCAT_DATASETTE_PORT:-8001} \
    --metadata metadata.yml \
    --reload \
    --template-dir=templates \
    --plugins-dir=plugins/ \
    --static assets:static \
    --setting sql_time_limit_ms 10000 \
    --setting facet_time_limit_ms 10000 \
    --setting num_sql_threads 10 \
    --setting max_csv_mb 12000 \
