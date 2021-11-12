{ pkgs ? import <nixpkgs> { }
, pkgsLinux ? import <nixpkgs> { system = "x86_64-linux"; }
}:

let

kazlauskas = pkgs.haskellPackages.callPackage ./kazlauskas.nix {};

site = pkgs.runCommand "www.kazlauskas.me" {} ''
    cp -rv ${./pages} ./pages
    cp -rv ${./templates} ./templates
    cp -rv ${./images} ./images
    cp -rv ${./entries} ./entries
    cp -rv ${./data} ./data
    cat ./data/sandwich.14.css
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
