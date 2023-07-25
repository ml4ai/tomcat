#!/usr/bin/env bash
# Shell script for launching the Datasette instance in dev mode

set -euo pipefail
datasette -i \
    /space/${USER}/tomcat/tomcat.db \
    --metadata metadata.yml \
    --reload \
    --template-dir=templates \
    --plugins-dir=plugins/ \
    --inspect-file=inspect-data.json \
    --static assets:static \
    --setting sql_time_limit_ms 10000 \
    --setting facet_time_limit_ms 10000 \
    --setting num_sql_threads 10 \
    --setting max_csv_mb 12000 \
