{ nixpkgs ? import ./nix/nixpkgs.nix
}:
let
  overlay = self: super: {
    haskellPackages = super.haskellPackages.override (old: {
      overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
        ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
        ghcWithPackages = hself.ghc.withPackages;
        # Any overrides or snowflake packages can be placed here
      });
    });
  };

  pkgs = import nixpkgs {
    # Yay, overlays!
    overlays = [overlay];
  };

  drv = pkgs.haskellPackages.callPackage ./hedgehog-webdriver.nix {};

  drvWithTools = pkgs.haskell.lib.addBuildTools drv
    [ # The selenium server for our webdriver instructions
      # Require version 2.53.1 as 3.x.x isn't supported yet
      (import ./nix/selenium-server-2.nix { inherit nixpkgs; })
    ];
in
  # Remove the need for 'shell.nix'.
  pkgs.haskell.lib.shellAware drvWithTools
