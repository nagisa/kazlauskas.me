{ stdenv, lib, makeWrapper
, woff2, harfbuzz
, haskellPackages
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
  executableHaskellDepends = with haskellPackages; [
    base binary bytestring containers hakyll hjsmin hyphenation pandoc
    pandoc-types tagsoup time xml
  ];
  buildTools = [ makeWrapper ];

  postInstall = ''
    mkdir -p $out/bin
    install src/make-it-woff2 $out/bin/make-it-woff2
    wrapProgram $out/bin/make-it-woff2 \
      --prefix PATH : ${lib.makeBinPath [ woff2 ]}
    wrapProgram $out/bin/kazlauskas \
      --prefix PATH : ${lib.makeBinPath [ harfbuzz.dev ]}
  '';

  homepage = "http://kazlauskas.me";
  description = "My hakyll blog";
  license = lib.licenses.mit;
}
