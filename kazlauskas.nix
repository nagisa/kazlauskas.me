{ mkDerivation, base, binary, bytestring, containers, hakyll
, hjsmin, hyphenation, pandoc, pandoc-types, stdenv, tagsoup, time
, xml
}:
mkDerivation {
  pname = "kazlauskas";
  version = "0.4.0.0";
  src = ./.;
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
