#!/usr/bin/env bash

set -e

./bash/scale.sh
stack build
stack exec build
if [[ -d /media/ftp/maximilian-huber.de ]]; then
    \cp -r ~/maximilian-huber/_site/* /media/ftp/maximilian-huber.de
fi
