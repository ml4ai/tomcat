#!/bin/bash

# This script installs SBCL, Quicklisp, and SHOP3 for MacPorts users with the
# installation locations (i.e. MacPorts lives in /opt/local and that you
# are fine with installing Quicklisp to ~/quicklisp). To change either of these
# variables, change the MACPORTS_PREFIX or QUICKLISP_DIR variables below.

# Invocation: ./setup_shop3.sh

MACPORTS_PREFIX=/opt/local
QUICKLISP_DIR=~/quicklisp

# Install sbcl and Quicklisp using MacPorts
sudo port install sbcl cl-quicklisp
if [[ $? -ne 0 ]]; then exit 1; fi;

sbcl --load "${MACPORTS_PREFIX}/share/cl-quicklisp/quicklisp.lisp" \
     --eval "(quicklisp-quickstart:install)" \
     --quit
if [[ $? -ne 0 ]]; then exit 1; fi;

sbcl --load "${QUICKLISP_DIR}/setup.lisp" \
     --eval "(ql:add-to-init-file)" \
     --quit
if [[ $? -ne 0 ]]; then exit 1; fi;

pushd ${QUICKLISP_DIR}
  mkdir -p local-projects
  pushd local-projects
    git clone --recurse-submodules https://github.com/shop-planner/shop3
    if [[ $? -ne 0 ]]; then exit 1; fi;
  popd
popd
