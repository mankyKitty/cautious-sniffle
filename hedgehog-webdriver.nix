{ mkDerivation, attoparsec, base, bytestring, containers, hedgehog
, lens, mtl, natural, stdenv, text, waargonaut, webdriver
}:
mkDerivation {
  pname = "hedgehog-webdriver";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers hedgehog lens mtl natural
    text waargonaut webdriver
  ];
  testHaskellDepends = [ base hedgehog webdriver ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
