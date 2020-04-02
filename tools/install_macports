#!/bin/bash

install_macports() {
  local version=2.6.2
  curl -O https://distfiles.macports.org/MacPorts/MacPorts-$version.tar.bz2
  if [[ $? -ne 0 ]]; then exit 1; fi
  if ! tar xf MacPorts-$version.tar.bz2; then exit 1; fi
  pushd MacPorts-$version > /dev/null
    if ! ./configure; then exit 1; fi
    if ! make -j; then exit 1; fi
    if ! sudo make -j install; then exit 1; fi
  popd > /dev/null
  /bin/rm -rf Macports-$version*
}

install_macports
