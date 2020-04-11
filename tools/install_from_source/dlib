#!/bin/bash

# Install DLib from source

# We download a specific commit snapshot of dlib from Github that
# contains a fix for the latest version of OpenCV that is being
# installed by Homebrew.
commit_sha=471c3d30e181a40942177a4358aa0496273d2108
curl -L https://github.com/davisking/dlib/archive/${commit_sha}.zip -o dlib.zip
unzip -qq dlib.zip
mv dlib-${commit_sha} dlib
pushd dlib
  mkdir build
  pushd build
    cmake ..
    cmake --build . --config Release
    if [[ $? -ne 0 ]]; then exit 1; fi;
    make -j8
    if [[ $? -ne 0 ]]; then exit 1; fi;
    sudo make install
    if [[ $? -ne 0 ]]; then exit 1; fi;
  popd
popd
exit 0
