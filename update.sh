#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"
root="$PWD"

(
    cd "$HOME/imgwork/02-galerie"
    ~/myconfig/myphoto/myphoto-gallery.sh export "$root/v2"
)

./sync-ftp.sh