{ mkDerivation, base, bytestring, Cabal, cabal-doctest, containers
, doctest, stdenv, unordered-containers, zlib
}:
mkDerivation {
  pname = "hyphenation";
  version = "0.7.1";
  sha256 = "1h5i07v2zlka29dj4zysc47p747j88x6z4zm3zwcr5i8yirm0p52";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base bytestring containers unordered-containers zlib
  ];
  testHaskellDepends = [
    base containers doctest unordered-containers
  ];
  homepage = "http://github.com/ekmett/hyphenation";
  description = "Configurable Knuth-Liang hyphenation";
  license = stdenv.lib.licenses.bsd3;
}
