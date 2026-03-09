#!/usr/bin/env nix-shell
#! nix-shell -i bash -p lftp pass

set -euo pipefail

ROOT="$(pwd)"

#######################################
# Config – EDIT THESE
#######################################

REMOTE_DIR="/maximilian-huber.de"
LOCAL_DIR="$ROOT/v2"

PASS_ENTRY="ftp.maximilian-huber.de"

PARALLEL=4
NET_TIMEOUT=20
MAX_RETRIES=3

if [[ ! -d "$LOCAL_DIR" ]]; then
  echo "Error: LOCAL_DIR '$LOCAL_DIR' does not exist" >&2
  exit 1
fi

#######################################
# Script
#######################################

set_vars_from_pass() {
  set +x
  local pass_output="$(pass "$PASS_ENTRY")"
  PASSWORD="$(echo "$pass_output" | head -1)"
  REMOTE_HOST="$(echo "$pass_output" | sed -n 's/^REMOTE_HOST=//p')"
  REMOTE_USER="$(echo "$pass_output" | sed -n 's/^REMOTE_USER=//p')"
}

set_vars_from_pass

if [[ -z "$PASSWORD" ]]; then
  echo "Error: empty password retrieved from pass entry '$PASS_ENTRY'" >&2
  exit 1
fi
if [[ -z "$REMOTE_HOST" ]]; then
  echo "Error: REMOTE_HOST not found in pass entry '$PASS_ENTRY'" >&2
  exit 1
fi
if [[ -z "$REMOTE_USER" ]]; then
  echo "Error: REMOTE_USER not found in pass entry '$PASS_ENTRY'" >&2
  exit 1
fi

cat <<EOF
sync $LOCAL_DIR to $REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR
  with 
    $PARALLEL parallel connections
    timeout $NET_TIMEOUT seconds
    max retries $MAX_RETRIES
EOF

lftp <<EOF
set net:timeout $NET_TIMEOUT
set net:max-retries $MAX_RETRIES
set net:persist-retries 1

# speed / safety options
set ftp:use-feat yes
set ftp:sync-mode yes
set ftp:use-mdtm yes
set file:charset UTF-8

# open SFTP connection
open -u $REMOTE_USER,$PASSWORD ftp://$REMOTE_HOST

mirror \
  --reverse \
  --delete \
  --only-newer \
  --use-cache \
  --verbose=1 \
  --parallel=$PARALLEL \
  "$LOCAL_DIR" "$REMOTE_DIR"

bye
EOF
