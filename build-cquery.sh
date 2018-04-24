#!/usr/bin/env bash

mkdir -p builds/cquery
mkdir -p builds/cquery/build

cd builds/cquery

if [ -d "cquery" ]; then
    cd cquery
    git pull --recurse-submodules
    cd  ..
else
    git clone \
        --depth 1 \
        --recurse-submodules \
        https://github.com/cquery-project/cquery.git
fi

cd build

cmake ../cquery \
      -DCMAKE_CXX_FLAGS="-march=native" \
      -DCMAKE_BUILD_TYPE="Release" \
      -DCMAKE_INSTALL_PREFIX="../install" \
      -G"Ninja"

ninja

ninja install

mkdir -p $HOME/bin

ln -sf $HOME/.emacs.d/builds/cquery/install/bin/* $HOME/bin/
