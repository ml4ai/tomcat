#!/bin/bash

# Install nlohmann-json from source.

version=3.7.3
wget https://github.com/nlohmann/json/archive/v$version.tar.gz
tar -xf v$version.tar.gz  
rm v$version.tar.gz

pushd json-$version > /dev/null
  mkdir build
  cd build
  cmake .. -DJSON_BuildTests=OFF
  sudo make -j install
popd > /dev/null

rm -rf json-$version

exit 0
