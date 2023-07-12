#!/usr/bin/env bash
# Shell script for launching the Datasette instance in dev mode

set -euo pipefail
datasette \
    -i /space/adarsh/tomcat/tomcat.db \
    --metadata metadata.yml \
    --reload \
    --template-dir=templates \
    --plugins-dir=plugins/ \
    --inspect-file=inspect-data.json
