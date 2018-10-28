#!/bin/sh

cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
./bash/scale.sh
cabal run build && ./bash/diffDirs.sh && ./bash/uploadToFTP.sh
