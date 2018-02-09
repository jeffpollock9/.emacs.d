#!/usr/bin/env bash

mkdir -p builds/rtags
mkdir -p builds/rtags/build

cd builds/rtags

git clone \
    --depth 1 \
    --recurse-submodules \
    https://github.com/Andersbakken/rtags.git

cd build

cmake ../rtags \
      -DCMAKE_CXX_FLAGS="-march=native" \
      -DCMAKE_BUILD_TYPE="Release" \
      -DCMAKE_INSTALL_PREFIX="../install" \
      -G"Ninja"

ninja

ninja install
