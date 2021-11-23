{ pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/2cf9db0e3d45b9d00f16f2836cb1297bcadc475e.tar.gz") { } }:

let
lib = pkgs.lib;
stdenv = pkgs.stdenv;

in

stdenv.mkDerivation {
  name = "www.kazlauskas.me";
  src = lib.cleanSourceWith {
    src = ./.;
    filter = (name: type:
      let relPath = lib.removePrefix (toString ./. + "/") (toString name);
          isInteresting = path: prefix: lib.hasPrefix prefix path;
      in lib.any (isInteresting relPath) [
        "pages" "templates" "images" "entries" "data" "_redirects" "_headers"
      ]
    );
  };
  buildInputs = [ (pkgs.haskellPackages.callPackage ./kazlauskas.nix {}) ];
  outputs = ["out"];
  buildPhase = ''
    ls -alh
    kazlauskas build
  '';
  installPhase = ''
    mv -v _site/ $out
  '';
}
