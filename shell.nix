{ pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/2cf9db0e3d45b9d00f16f2836cb1297bcadc475e.tar.gz") { } }:

let
  kazlauskas = pkgs.haskellPackages.callPackage ./kazlauskas.nix {};

in pkgs.mkShell {
  buildInputs = [
    kazlauskas
  ];
}
