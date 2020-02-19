{ mkDerivation, base, base64-bytestring, bifunctors, bytestring
, clay, constraints-extras, containers, contravariant
, dependent-map, dependent-sum, dependent-sum-template, errors
, exceptions, generics-sop, hedgehog, http-client, lens, linear
, modern-uri, mtl, natural, process, scientific, scotty
, semigroupoids, servant, servant-client, servant-client-core
, servant-waargonaut, stdenv, tasty, tasty-hedgehog, tasty-hunit
, text, time, vector, waargonaut, warp
}:
mkDerivation {
  pname = "cautious-sniffle";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring bifunctors bytestring clay
    constraints-extras containers contravariant dependent-map
    dependent-sum dependent-sum-template errors generics-sop
    http-client lens linear modern-uri mtl natural scientific
    semigroupoids servant servant-client servant-client-core
    servant-waargonaut text time vector waargonaut
  ];
  testHaskellDepends = [
    base base64-bytestring bifunctors bytestring clay
    constraints-extras containers contravariant dependent-map
    dependent-sum dependent-sum-template errors exceptions generics-sop
    hedgehog http-client lens linear modern-uri mtl natural process
    scientific scotty semigroupoids servant servant-client
    servant-client-core servant-waargonaut tasty tasty-hedgehog
    tasty-hunit text time vector waargonaut warp
  ];
  homepage = "https://github.com/mankyKitty/cautious-sniffle";
  description = "Servant client bindings for W3C Webdriver Protocol";
  license = stdenv.lib.licenses.bsd3;
}
