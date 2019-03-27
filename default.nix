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
  servant-waarg-overlay = import "${sources.servant-waargonaut}/servant-waargonaut-deps.nix";

  overlay = self: super: {
    gconf = super.gnome2.GConf;
    chromedriver = super.callPackage ./nix/selenium-server/chromedriver {};
    selenium-server-standalone = super.callPackage ./nix/selenium-server {};
    haskellPackages = super.haskellPackages.override (old: {
      overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
        ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
        ghcWithPackages = hself.ghc.withPackages;

        # Any overrides or snowflake packages can be placed here

        # webdriver = hself.callCabal2nix "webdriver" (import ./nix/swamp-webdriver.nix) {};
        # webdriver = hself.callPackage (import ../hs-webdriver) {};

        dependent-sum-template       = hself.callCabal2nix "dependent-sum-template" sources.dependent-sum-template {};
        dependent-sum       = hself.callCabal2nix "dependent-sum" sources.dependent-sum {};
        dependent-map       = hself.callCabal2nix "dependent-map" sources.dependent-map {};

        servant-client      = hself.callHackage "servant-client" "0.14" {};
        servant-client-core = hself.callHackage "servant-client-core" "0.14.1" {};

        waargonaut         = hself.callPackage sources.waargonaut {};
        servant-waargonaut = hself.callCabal2nix "servant-waargonaut" sources.servant-waargonaut {};
      });
    });
  };

  pkgs = import nixpkgs {
    # Yay, overlays!
    overlays =
      [ waarg-overlay
        servant-waarg-overlay
        overlay
      ];
  };

  drv = pkgs.haskellPackages.callPackage ./hedgehog-webdriver.nix {};

  drvWithTools = pkgs.haskell.lib.addBuildTools drv
    [ # The selenium server for our webdriver instructions
      # Require version 2.53.1 as 3.x.x isn't supported yet
      # (import ./nix/selenium-server-2.nix { inherit nixpkgs; })
      # (pkgs.callPackage (import ./nix/selenium-standalone-server.nix) {})
      pkgs.selenium-server-standalone
      pkgs.chromedriver
      pkgs.geckodriver
      pkgs.jre
    ];
in
  # Remove the need for 'shell.nix'.
  pkgs.haskell.lib.shellAware drvWithTools
