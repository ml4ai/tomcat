#!/usr/bin/env bash

# This script generates an up-to-date schema diagram for the ToMCAT database.
# It uses the visualize-sqlite tool
# (https://github.com/uhhhwaitwhat/visualize-sqlite).
# The tool can be installed by running 'cargo install visualize-sqlite'
# (assuming you have Cargo installed)
# Invocation: ./generate_database_diagram.sh <path_to_database>


export DATABASE=$1

visualize-sqlite $DATABASE \
    | dot \
    -Tpng \
    -Gfontname='Fira Mono'\
    -Gfontcolor='#586e75' \
    -Gbgcolor='#fdf6e3' \
    -Nfontname='Fira Mono'\
    -Nfontcolor='#586e75' \
    -Efontname='Fira Mono' > static/db_diagram.png
