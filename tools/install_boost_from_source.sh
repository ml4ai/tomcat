#!/bin/bash

# Install Boost from source

boost_minor_version=71
curl -LO\
  https://dl.bintray.com/boostorg/release/1."${boost_minor_version}".0/source/boost_1_"${boost_minor_version}"_0.tar.gz
if [[ $? -ne 0 ]]; then exit 1; fi;
tar xfz boost_1_"${boost_minor_version}"_0.tar.gz
if [[ $? -ne 0 ]]; then exit 1; fi;
pushd boost_1_"${boost_minor_version}"_0
  ./bootstrap.sh
  if [[ $? -ne 0 ]]; then exit 1; fi;
  sudo ./b2 install
  if [[ $? -ne 0 ]]; then exit 1; fi;
popd
/bin/rm -rf boost_1_"${boost_minor_version}"_0*
exit 0
