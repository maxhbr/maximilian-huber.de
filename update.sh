#!/usr/bin/env bash

set -e

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $ROOT

~/Bilder/00-galerie/updateFiles.sh "$ROOT/galerie"
stack build
stack exec build
if [[ -d /media/ftp/maximilian-huber.de ]]; then
    \cp -r $ROOT/_site/* /media/ftp/maximilian-huber.de
fi
