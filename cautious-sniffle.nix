{ mkDerivation, attoparsec, base, base64-bytestring, bifunctors
, bytestring, clay, containers, contravariant, dependent-map
, dependent-sum, dependent-sum-template, directory, distributive
, errors, generics-sop, haskell-src-exts, hedgehog, http-client
, lens, modern-uri, mtl, natural, pretty, process, scientific
, scotty, semigroupoids, servant, servant-client
, servant-waargonaut, stdenv, template-haskell, text, vector
, waargonaut, webdriver
}:
mkDerivation {
  pname = "cautious-sniffle";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bifunctors bytestring clay
    containers contravariant dependent-map dependent-sum
    dependent-sum-template distributive errors generics-sop
    haskell-src-exts hedgehog http-client lens modern-uri mtl natural
    pretty scientific semigroupoids servant servant-client
    servant-waargonaut template-haskell text vector waargonaut
    webdriver
  ];
  executableHaskellDepends = [
    base bytestring directory lens text waargonaut
  ];
  testHaskellDepends = [
    base clay containers dependent-map dependent-sum hedgehog
    http-client lens modern-uri process scotty servant-client text
    vector waargonaut webdriver
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
