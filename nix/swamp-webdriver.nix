let
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    hs-webdriver-pinned = initialNixpkgs.pkgs.lib.importJSON ./swamp-webdriver.json;
    hs-webdriver = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "swamp-agr";
      repo = "hs-webdriver";
      inherit (hs-webdriver-pinned) rev sha256;
    };
  };
in
  sources.hs-webdriver
