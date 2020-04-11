#!/bin/bash

# Install OpenCV 4 from source

opencv_version=4.2.0
wget https://github.com/opencv/opencv/archive/$opencv_version.tar.gz && \
if [[ $? -ne 0 ]]; then exit 1; fi;
tar xfz $opencv_version.tar.gz
if [[ $? -ne 0 ]]; then exit 1; fi;

pushd opencv-$opencv_version
  mkdir build
  pushd build
    cmake -D CMAKE_BUILD_TYPE=RELEASE -D OPENCV_GENERATE_PKGCONFIG=ON ..
    if [[ $? -ne 0 ]]; then exit 1; fi;
    make -j8
    if [[ $? -ne 0 ]]; then exit 1; fi;
    sudo make install
    if [[ $? -ne 0 ]]; then exit 1; fi;
    pkg-config --modversion opencv4
    if [[ $? -ne 0 ]]; then exit 1; fi;
  popd
popd
