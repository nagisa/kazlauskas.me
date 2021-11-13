{ pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/2cf9db0e3d45b9d00f16f2836cb1297bcadc475e.tar.gz") { } }:

let
lib = pkgs.lib;
stdenv = pkgs.stdenv;

site = stdenv.mkDerivation {
  name = "www.kazlauskas.me";
  src = lib.cleanSourceWith {
    src = ./.;
    filter = (name: type:
      let relPath = lib.removePrefix (toString ./. + "/") (toString name);
          isInteresting = path: prefix: lib.hasPrefix prefix path;
      in lib.any (isInteresting relPath) [
        "pages" "templates" "images" "entries" "data" "redirects.conf"
      ]
    );
  };
  buildInputs = [ (pkgs.haskellPackages.callPackage ./kazlauskas.nix {}) ];
  outputs = ["images" "data" "out"];
  buildPhase = ''
    ls -alh
    kazlauskas build
  '';
  installPhase = ''
    mkdir -p $images/var/www $data/var/www $out/var/
    mv -v _site/images $images/var/www/images
    mv -v _site/data $data/var/www/data
    mv -v _site/ $out/var/www
  '';
};

nginxConf = pkgs.runCommand "nginxConf" { preferLocalBuild = true; }
  "mkdir $out && cp -vp ${./nginx.conf} $out/nginx.conf";

shadow = pkgs.dockerTools.buildImage {
  name = "shadow";
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    ${pkgs.dockerTools.shadowSetup}
    groupadd -r nginx
    useradd -r -g nginx nginx
    mkdir /nginx
    mkdir -p /var/cache/nginx /var/log/nginx
    chown nginx:nginx /nginx /var/cache/nginx /var/log/nginx
  '';
};

in pkgs.dockerTools.streamLayeredImage {
  name = "kazlauskas-me";
  tag = "latest";
  fromImage = shadow;
  contents = [pkgs.nginx nginxConf site.images site.data site.out];
  config = {
    Cmd = [ "${pkgs.nginx}/bin/nginx" "-c" "${nginxConf}/nginx.conf" "-p" "/nginx" ];
    User = "nginx";
  };
}
