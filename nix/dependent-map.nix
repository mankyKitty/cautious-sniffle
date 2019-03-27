let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    dependent-map-pinned = initialNixpkgs.pkgs.lib.importJSON ./dependent-map.json;
    dependent-map = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "mokus0";
      repo = "dependent-map";
      inherit (dependent-map-pinned) rev sha256;
    };
  };
in
  sources.dependent-map
