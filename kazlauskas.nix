{ stdenv, haskellPackages, base, binary, bytestring, containers, hakyll
, hjsmin, hyphenation, pandoc, pandoc-types, tagsoup, time
, xml
}:

haskellPackages.mkDerivation {
  pname = "kazlauskas";
  version = "0.4.0.0";
  src = stdenv.lib.sourceFilesBySuffices ./. [ ".hs" ".cabal" "LICENCE" ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring containers hakyll hjsmin hyphenation pandoc
    pandoc-types tagsoup time xml
  ];
  homepage = "http://kazlauskas.me";
  description = "My hakyll blog";
  license = stdenv.lib.licenses.mit;
}
