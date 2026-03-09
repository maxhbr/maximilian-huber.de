#!/usr/bin/env nix-shell
#! nix-shell -i bash -p gftp pass

set -euo pipefail

PASS_ENTRY="ftp.maximilian-huber.de"

#######################################
# Read credentials from pass
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

#######################################
# Launch gftp
#######################################

exec gftp "ftp://${REMOTE_USER}:${PASSWORD}@${REMOTE_HOST}/"
