#!/usr/bin/env bash

set -xe

workdir=$(mktemp -d)
trap 'rm -rf "$workdir"' EXIT

curl_retries=5
while ! curl -o "$workdir/install" -v --fail -L "${INPUT_INSTALL_URL:-https://nixos.org/nix/install}"
do
  sleep 1
  ((curl_retries--))
  if [[ $curl_retries -le 0 ]]; then
    echo "curl retries failed" >&2
    exit 1
  fi
done

mkdir -m 0755 /nix && chown buildbot /nix
sh "$workdir/install" --no-channel-add
source $HOME/.nix-profile/etc/profile.d/nix.sh

nix-build
