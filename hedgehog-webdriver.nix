{ mkDerivation, algebraic-graphs, attoparsec, base, bytestring
, containers, generic-trie, hedgehog, lens, mtl, natural, stdenv
, text, waargonaut, webdriver
}:
mkDerivation {
  pname = "hedgehog-webdriver";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    algebraic-graphs attoparsec base bytestring containers generic-trie
    hedgehog lens mtl natural text waargonaut webdriver
  ];
  testHaskellDepends = [ base hedgehog webdriver ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
