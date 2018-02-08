#!/usr/bin/env bash

mkdir -p builds/doxymacs

cd builds/doxymacs

wget https://kent.dl.sourceforge.net/project/doxymacs/doxymacs/1.8.0/doxymacs-1.8.0.tar.gz

tar xf doxymacs-1.8.0.tar.gz

cd doxymacs-1.8.0

./configure --prefix "`pwd`/../install"

make
make install
