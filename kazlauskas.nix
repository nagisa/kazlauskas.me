{ stdenv, lib, haskellPackages, base, binary, bytestring, containers, hakyll
, hjsmin, hyphenation, pandoc, pandoc-types, tagsoup, time
, xml
}:

haskellPackages.mkDerivation {
  pname = "kazlauskas";
  version = "0.4.0.0";
  src = lib.sourceFilesBySuffices ./. [ ".hs" ".cabal" "LICENCE" ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring containers hakyll hjsmin hyphenation pandoc
    pandoc-types tagsoup time xml
  ];
  homepage = "http://kazlauskas.me";
  description = "My hakyll blog";
  license = lib.licenses.mit;
}
