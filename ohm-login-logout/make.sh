#!/bin/sh

cd /home/k/q/ohm-examples.git/ohm-login-logout
nix-shell \
  -I nixpkgs=/nix/var/nix/profiles/per-user/k/channels/nixpkgs-unstable \
  -I . \
  shell.nix \
  --command 'cabal configure --ghcjs && cabal build'
