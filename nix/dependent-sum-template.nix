let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    dependent-sum-template-pinned = initialNixpkgs.pkgs.lib.importJSON ./dependent-sum-template.json;
    dependent-sum-template = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "mokus0";
      repo = "dependent-sum-template";
      inherit (dependent-sum-template-pinned) rev sha256;
    };
  };
in
  sources.dependent-sum-template
