#!/usr/bin/env bash

mkdir -p builds/rtags
mkdir -p builds/rtags/build

cd builds/rtags

if [ -d "rtags" ]; then
    cd rtags
    git pull --recurse-submodules
    cd  ..
else
    git clone \
        --depth 1 \
        --recurse-submodules \
        https://github.com/Andersbakken/rtags.git
fi

cd build

cmake ../rtags \
      -DCMAKE_CXX_FLAGS="-march=native" \
      -DCMAKE_BUILD_TYPE="Release" \
      -DCMAKE_INSTALL_PREFIX="../install" \
      -G"Ninja"

ninja

ninja install

mkdir -p $HOME/bin

ln -sf $HOME/.emacs.d/builds/rtags/install/bin/* $HOME/bin/
