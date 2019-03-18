{ mkDerivation, attoparsec, base, bytestring, containers, directory
, distributive, haskell-src-exts, hedgehog, http-client, lens, mtl
, natural, pretty, scientific, servant, servant-client
, servant-waargonaut, stdenv, template-haskell, text, vector
, waargonaut, webdriver
}:
mkDerivation {
  pname = "hedgehog-webdriver";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers distributive haskell-src-exts
    hedgehog http-client lens mtl natural pretty scientific servant
    servant-client servant-waargonaut template-haskell text vector
    waargonaut webdriver
  ];
  executableHaskellDepends = [
    base bytestring directory lens text waargonaut
  ];
  testHaskellDepends = [ base hedgehog webdriver ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
