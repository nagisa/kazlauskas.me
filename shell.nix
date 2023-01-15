{ pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/469aec905bab3be98838c7eb996ceffb2ea44404.tar.gz") { } }:

let
  kazlauskas = pkgs.haskellPackages.callPackage ./kazlauskas.nix {};

in pkgs.mkShell {
  buildInputs = [
    kazlauskas
  ];
}
