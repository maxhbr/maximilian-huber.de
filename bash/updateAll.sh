#!/bin/sh

pushd /home/hubi/maximilian-huber
./bash/scale.sh
cabal run maximilian-huber
./bash/diffDirs.sh
./bash/uploadToFTP.sh
popd
