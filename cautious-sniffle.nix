{ mkDerivation, attoparsec, base, base64-bytestring, bifunctors
, bytestring, clay, containers, contravariant, dependent-map
, dependent-sum, dependent-sum-template, directory, distributive
, errors, exceptions, generics-sop, haskell-src-exts, hedgehog
, http-client, lens, modern-uri, mtl, natural, pretty, process
, scientific, scotty, semigroupoids, servant, servant-client
, servant-client-core, servant-waargonaut, stdenv, tasty
, tasty-hedgehog, tasty-hunit, template-haskell, text, time, vector
, waargonaut, warp
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
    servant-client-core servant-waargonaut template-haskell text time
    vector waargonaut
  ];
  executableHaskellDepends = [
    base bytestring directory lens text waargonaut
  ];
  testHaskellDepends = [
    base clay containers dependent-map dependent-sum exceptions
    hedgehog http-client lens modern-uri mtl process scotty servant
    servant-client servant-client-core tasty tasty-hedgehog tasty-hunit
    text vector waargonaut warp
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
