#!/usr/bin/env bash

virtualenv -p python3 ${WORKON_HOME}/emacs-dev

${WORKON_HOME}/emacs-dev/bin/pip install --upgrade -r requirements.txt
