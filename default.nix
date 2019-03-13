{ nixpkgs ? import ./nix/nixpkgs.nix
}:
let
  sources = {
    waargonaut         = import ./nix/waargonaut.nix;
    servant-waargonaut = import ./nix/servant-waargonaut.nix;
  };

  waarg-overlay         = import "${sources.waargonaut}/waargonaut-deps.nix";
  servant-waarg-overlay = import "${sources.servant-waargonaut}/servant-waargonaut-deps.nix";

  overlay = self: super: {
    haskellPackages = super.haskellPackages.override (old: {
      overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
        ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
        ghcWithPackages = hself.ghc.withPackages;
        # Any overrides or snowflake packages can be placed here
        # webdriver = hself.callCabal2nix "webdriver" (import ./nix/swamp-webdriver.nix) {};
        # webdriver = hself.callPackage (import ../hs-webdriver) {};
        servant-client      = hself.callHackage "servant-client" "0.14" {};
        servant-client-core = hself.callHackage "servant-client-core" "0.14.1" {};

        waargonaut         = hself.callPackage sources.waargonaut {};
        servant-waargonaut = hself.callCabal2nix "servant-waargonaut" sources.servant-waargonaut {};
      });
    });
  };

  pkgs = import nixpkgs {
    # Yay, overlays!
    overlays = [waarg-overlay servant-waarg-overlay overlay];
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
      pkgs.openjdk
    ];
in
  # Remove the need for 'shell.nix'.
  pkgs.haskell.lib.shellAware drvWithTools
