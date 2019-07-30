{ nixpkgs ? import ./nix/nixpkgs.nix
}:
let
  sources = {
    waargonaut             = import ./nix/waargonaut.nix;
    servant-waargonaut     = import ./nix/servant-waargonaut.nix;
    dependent-map          = import ./nix/dependent-map.nix;
    dependent-sum          = import ./nix/dependent-sum.nix;
    dependent-sum-template = import ./nix/dependent-sum-template.nix;
  };

  waarg-overlay         = import "${sources.waargonaut}/waargonaut-deps.nix";

  overlay = self: super: {
    gconf = super.gnome2.GConf;
    chromedriver = super.callPackage ./nix/selenium-server/chromedriver {};
    selenium-server-standalone = super.callPackage ./nix/selenium-server {};
    haskellPackages = super.haskellPackages.override (old: {
      overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
        ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
        ghcWithPackages = hself.ghc.withPackages;

        # Any overrides or snowflake packages can be placed here
        hedgehog = hsuper.callHackageDirect {
          pkg = "hedgehog";
          ver = "1.0";
          sha256 = "06q1w1pjvhdr6za1n5kjd3zszh4xi2ixrwgclqqqj6nhdiz8y6zj";
        } {};

        clay = super.haskell.lib.dontCheck hsuper.clay;
        text-utf8 = super.haskell.lib.dontCheck hsuper.text-utf8;

        dependent-sum-template = hsuper.callCabal2nix "dependent-sum-template" sources.dependent-sum-template {};
        dependent-sum       = hsuper.callCabal2nix "dependent-sum" sources.dependent-sum {};
        dependent-map       = hsuper.callCabal2nix "dependent-map" sources.dependent-map {};
        waargonaut         = hsuper.callCabal2nix "waargonaut" sources.waargonaut {};
        servant-waargonaut = hsuper.callCabal2nix "servant-waargonaut" sources.servant-waargonaut {};

      });
    });
  };

  pkgs = import nixpkgs {
    config.allowBroken = true;
    # Yay, overlays!
    overlays = [waarg-overlay overlay];
  };

  drv = pkgs.haskellPackages.callPackage ./cautious-sniffle.nix {};

  drvWithTools = pkgs.haskell.lib.addBuildTools drv
    [ # The selenium server for our webdriver instructions
      # Require version 2.53.1 as 3.x.x isn't supported yet
      pkgs.selenium-server-standalone
      pkgs.chromedriver
      pkgs.geckodriver
      pkgs.jre
    ];
in
  drvWithTools.env
