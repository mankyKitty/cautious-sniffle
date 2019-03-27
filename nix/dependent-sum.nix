let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    dependent-sum-pinned = initialNixpkgs.pkgs.lib.importJSON ./dependent-sum.json;
    dependent-sum = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "mokus0";
      repo = "dependent-sum";
      inherit (dependent-sum-pinned) rev sha256;
    };
  };
in
  sources.dependent-sum
