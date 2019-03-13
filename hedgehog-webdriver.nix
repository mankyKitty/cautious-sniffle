{ mkDerivation, attoparsec, base, bytestring, containers, directory
, hedgehog, http-client, lens, mtl, natural, scientific
, servant-client, servant-waargonaut, stdenv, text, vector
, waargonaut, webdriver
}:
mkDerivation {
  pname = "hedgehog-webdriver";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers hedgehog http-client lens mtl
    natural scientific servant-client servant-waargonaut text vector
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
