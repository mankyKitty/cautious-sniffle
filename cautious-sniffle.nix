{ mkDerivation, base, base64-bytestring, bifunctors, bytestring
, clay, containers, contravariant, dependent-map, dependent-sum
, dependent-sum-template, errors, exceptions, generics-sop
, hedgehog, http-client, lens, modern-uri, mtl, natural, process
, resourcet, scientific, scotty, semigroupoids, servant
, servant-client, servant-client-core, servant-waargonaut, stdenv
, tasty, tasty-hedgehog, tasty-hunit, text, text-icu, time, vector
, waargonaut, warp
}:
mkDerivation {
  pname = "cautious-sniffle";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring bifunctors bytestring clay containers
    contravariant dependent-map dependent-sum dependent-sum-template
    errors generics-sop http-client lens modern-uri mtl natural
    resourcet scientific semigroupoids servant servant-client
    servant-client-core servant-waargonaut text text-icu time vector
    waargonaut
  ];
  testHaskellDepends = [
    base base64-bytestring bifunctors bytestring clay containers
    contravariant dependent-map dependent-sum dependent-sum-template
    errors exceptions generics-sop hedgehog http-client lens modern-uri
    mtl natural process resourcet scientific scotty semigroupoids
    servant servant-client servant-client-core servant-waargonaut tasty
    tasty-hedgehog tasty-hunit text text-icu time vector waargonaut
    warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
