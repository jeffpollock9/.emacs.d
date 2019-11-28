#!/usr/bin/env bash

mkdir -p builds/ccls
mkdir -p builds/ccls/build

cd builds/ccls

if [ -d "ccls" ]; then
    cd ccls
    git pull --recurse-submodules
    cd  ..
else
    git clone --recurse-submodules https://github.com/MaskRay/ccls.git
fi

cd build

cmake ../ccls \
      -DCMAKE_CXX_COMPILER="g++-9" \
      -DCMAKE_CXX_FLAGS="-march=native" \
      -DCMAKE_BUILD_TYPE="Release" \
      -DCMAKE_INSTALL_PREFIX="../install" \
      -DUSE_SYSTEM_RAPIDJSON=OFF \
      -DCMAKE_PREFIX_PATH="/opt/clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04" \
      -G"Ninja"

ninja
ninja install

ln -sf $HOME/.emacs.d/builds/ccls/install/bin/ccls $HOME/bin/
