#!/usr/bin/env bash

virtualenv -p python3 ${WORKON_HOME}/pymacs

${WORKON_HOME}/pymacs/bin/pip install --upgrade -r requirements.txt
