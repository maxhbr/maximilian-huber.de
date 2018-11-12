#!/usr/bin/env bash

set -e

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $ROOT

~/Bilder/00-galerie/updateFiles.sh "$ROOT/galerie"
stack build
stack exec build

################################################################################
# update Files
INPUT="$ROOT/_site/"
MIRROR="$ROOT/_site-ftp-mirror/"
TARGET="/media/ftp/maximilian-huber.de"
if [[ ! -d "$TARGET" ]]; then
    mountFTP.sh
fi

echo "update files ..."

if [[ ! -d "$INPUT" ]]; then
    exit 1
fi

mkdir -p "$MIRROR"

# make dirs
mydirs=$(cd "$INPUT"; find . \( ! -regex '.*/\..*' \) -type d)
for item in $mydirs ; do
    if [[ -d "$MIRROR/$item" ]]; then
        continue
    fi
    mkdir -p "$TARGET/$item" || true
    mkdir -p "$MIRROR/$item"
done

# copy files
myfiles=$(cd "$INPUT"; find . \( ! -regex '.*/\..*' \) -type f)
for item in $myfiles; do
    if [ -f "$MIRROR$item" ]; then
        a=$(md5sum "$INPUT/$item" | awk '{print $1}')
        b=$(md5sum "$MIRROR/$item" | awk '{print $1}')
        if [ "$b" == "$a" ] ; then
            continue
        fi
    fi
    cp -vf "$INPUT/$item" "$TARGET/$item" || true
    cp     "$INPUT/$item" "$MIRROR/$item"
done

# remove outdated
mirrorfiles=$(cd "$MIRROR"; find . \( ! -regex '.*/\..*' \) -type f)
for item in $mirrorfiles; do
    if [[ ! -f "$INPUT/$item" ]]; then
        rm -v "$TARGET/$item" || true
        rm    "$MIRROR/$item"
    fi
done
