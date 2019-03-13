let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    servant-waargonaut-pinned = initialNixpkgs.pkgs.lib.importJSON ./servant-waargonaut.json;
    servant-waargonaut = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "servant-waargonaut";
      inherit (servant-waargonaut-pinned) rev sha256;
    };
  };
in
  sources.servant-waargonaut
