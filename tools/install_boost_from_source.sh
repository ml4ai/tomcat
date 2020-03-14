#!/bin/bash

# Install Boost from source

curl -O https://dl.bintray.com/boostorg/release/1.71.0/source/boost_1_71_0.tar.gz
if [[ $? -ne 0 ]]; then exit 1; fi;
tar xfz boost_1_71_0.tar.gz
if [[ $? -ne 0 ]]; then exit 1; fi;
pushd boost_1_71_0
  ./bootstrap.sh
  if [[ $? -ne 0 ]]; then exit 1; fi;
  sudo ./b2 install
  if [[ $? -ne 0 ]]; then exit 1; fi;
popd
rm -rf boost_1_71_0*
exit 0
