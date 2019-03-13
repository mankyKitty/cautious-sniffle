let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    uri-bytestring-pinned = initialNixpkgs.pkgs.lib.importJSON ./uri-bytestring.json;
    uri-bytestring = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "Soostone";
      repo = "uri-bytestring";
      inherit (uri-bytestring-pinned) rev sha256;
    };
  };
in
  sources.uri-bytestring