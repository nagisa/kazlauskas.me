{ stdenv, lib, haskellPackages, base, binary, bytestring, containers, hakyll
, hjsmin, hyphenation, pandoc, pandoc-types, tagsoup, time
, xml
}:

haskellPackages.mkDerivation {
  pname = "kazlauskas";
  version = "0.4.0.0";

  src = lib.cleanSourceWith {
    src = ./.;
    filter = (name: type:
      let relPath = lib.removePrefix (toString ./. + "/") (toString name);
          isInteresting = path: prefix: lib.hasPrefix prefix path;
      in lib.any (isInteresting relPath) ["kazlauskas.cabal" "Setup.hs" "src" "LICENCE"]
    );
  };

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
