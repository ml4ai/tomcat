#!/usr/bin/env bash

# Update inspect-data.json
datasette inspect /space/$USER/tomcat/tomcat.db > inspect-data.json
