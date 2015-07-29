#!/bin/sh

pushd $HOME/maximilian-huber
cabal2nix ./ > default.nix
cabal2nix --shell ./ > shell.nix
nix-shell -I ~ --command 'cabal configure --enable-tests'
popd
