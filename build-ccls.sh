#!/usr/bin/env bash

mkdir -p builds/ccls
mkdir -p builds/ccls/build

cd builds/ccls

if [ -d "ccls" ]; then
    cd ccls
    git pull --recurse-submodules
    cd  ..
else
    git clone \
        --depth 1 \
        --recurse-submodules \
        https://github.com/MaskRay/ccls.git
fi

cd build

cmake ../ccls \
      -DCMAKE_CXX_COMPILER="g++-8" \
      -DCMAKE_CXX_FLAGS="-march=native" \
      -DCMAKE_BUILD_TYPE="Release" \
      -DCMAKE_INSTALL_PREFIX="../install" \
      -DCLANG_ROOT="/usr/lib/llvm-7" \
      -G"Ninja"

ninja

ninja install

mkdir -p $HOME/bin

ln -sf $HOME/.emacs.d/builds/ccls/install/bin/* $HOME/bin/
