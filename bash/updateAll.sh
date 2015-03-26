#!/bin/sh

pushd $HOME/maximilian-huber
git add blog/*
./bash/scale.sh
cabal run build && ./bash/diffDirs.sh && ./bash/uploadToFTP.sh
popd
