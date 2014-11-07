#!/bin/sh

pushd /home/hubi/maximilian-huber
./bash/scale.sh
cabal run build && ./bash/diffDirs.sh && ./bash/uploadToFTP.sh
popd
