{ pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/2cf9db0e3d45b9d00f16f2836cb1297bcadc475e.tar.gz") { } }:

let

kazlauskas = pkgs.haskellPackages.callPackage ./kazlauskas.nix {};

site = pkgs.runCommand "www.kazlauskas.me" {} ''
    cp -rvp ${./pages} ./pages
    cp -rvp ${./templates} ./templates
    cp -rvp ${./images} ./images
    cp -rvp ${./entries} ./entries
    cp -rvp ${./data} ./data
    ${kazlauskas}/bin/kazlauskas build
    mkdir -p $out/srv
    mv -v _site $out/srv/http
'';

goStatic = pkgs.dockerTools.pullImage {
  imageName = "pierrezemb/gostatic";
  imageDigest = "sha256:54a6f97f5806db11cd335fec9bc79dc9d64db7d1a47c5d94f89d852290c5753c";
  finalImageName = "gostatic";
  finalImageTag = "latest";
  sha256 = "sha256-8xrnZStz1T6+eBzVjy6hTJM0ijEuLb/PvDkQWa5DUok=";
  os = "linux";
  arch = "x86_64";
};

in pkgs.dockerTools.buildImage {
  name = "kazlauskas.me";
  tag = "latest";
  fromImage = goStatic;
  contents = site;
  config = {
    Cmd = [ "/goStatic" "-port=8080" ];
  };
}