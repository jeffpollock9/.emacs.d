#!/usr/bin/env bash

mkdir -p builds

cd builds

if [ -d "emacs-libvterm" ]; then
    cd emacs-libvterm
    git pull --recurse-submodules
    cd  ..
else
    git clone --recurse-submodules https://github.com/akermu/emacs-libvterm.git
fi

mkdir -p emacs-libvterm/build

cd emacs-libvterm/build

cmake .. \
      -DCMAKE_C_COMPILER="gcc" \
      -DCMAKE_C_FLAGS="-march=native" \
      -DCMAKE_BUILD_TYPE="Release" \
      -G"Ninja"

ninja libvterm
ninja
