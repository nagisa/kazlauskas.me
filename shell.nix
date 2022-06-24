{ pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/f4b359f751dada1c26ec3b8d62b7fe52ca44b2a6.tar.gz") { } }:

let
  kazlauskas = pkgs.haskellPackages.callPackage ./kazlauskas.nix {};

in pkgs.mkShell {
  buildInputs = [
    kazlauskas
  ];
}
