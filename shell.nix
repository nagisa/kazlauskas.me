{ nixpkgs ? import <nixpkgs> {} }:

let
  kazlauskas = nixpkgs.haskellPackages.callPackage ./kazlauskas.nix {};

in nixpkgs.mkShell {
    buildInputs = [
        kazlauskas
    ];
}
