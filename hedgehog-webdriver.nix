{ mkDerivation, base, hedgehog, stdenv, text, webdriver }:
mkDerivation {
  pname = "hedgehog-webdriver";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hedgehog text webdriver ];
  testHaskellDepends = [ base hedgehog webdriver ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
