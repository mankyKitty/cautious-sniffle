# { nixpkgs ? import ./nix/nixpkgs.nix
{ sources ? import ./nix/sources.nix
}:
let
  nivSrc = h: p: h.callCabal2nix p sources.${p} {};

  hwBiome = h: dontCheck: {
    hw-bits = dontCheck (nivSrc h "hw-bits");
    hw-excess = dontCheck (nivSrc h "hw-excess");
    hw-balancedparens = dontCheck (nivSrc h "hw-balancedparens");
    hw-rankselect-base = dontCheck (nivSrc h "hw-rankselect-base");
    hw-rankselect = dontCheck (nivSrc h "hw-rankselect");
    hw-json-standard-cursor = dontCheck (nivSrc h "hw-json-standard-cursor");
    hw-json = dontCheck (nivSrc h "hw-json");
  };

  overlay = self: super: {
    gconf = super.gnome2.GConf;
    chromedriver = super.callPackage ./nix/selenium-server/chromedriver {};
    selenium-server-standalone = super.callPackage ./nix/selenium-server {};

    haskellPackages = super.haskellPackages.override (old: {
      overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper:
      let
        dontCheck = super.haskell.lib.dontCheck;
        dep-sum = sources.dependent-sum;
      in
      (hwBiome hsuper dontCheck) // {
        ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
        ghcWithPackages = hself.ghc.withPackages;

        clay = hsuper.callCabal2nix "clay" sources.clay {};
        natural = super.haskell.lib.doJailbreak hsuper.natural;
        generic-lens = dontCheck (nivSrc hsuper "generic-lens");
        constraints-extras = nivSrc hsuper "constraints-extras";

        dependent-map = hsuper.callHackageDirect {
          pkg = "dependent-map";
          ver = "0.3";
          sha256 = "1r4n7ivbkrrm6h8s124gj23bjv2kcx5sb4bfp1hriqsng3fgkifi";
        } {};

        some = hsuper.callHackageDirect {
          pkg = "some";
          ver = "1.0.0.3";
          sha256 = "1cx5dq2p18v1ib1i10m7hx0w2rirjfc540r9barffbs8lyy3z7sf";
        } {};

        dependent-sum-template = hsuper.callHackageDirect {
          pkg = "dependent-sum-template";
          ver = "0.1.0.0";
          sha256 = "0fm73cbja570lfxznv66daya5anp4b0m24jjm5fwn95f49dp9d4n";
        } {};

        dependent-sum = hsuper.callHackageDirect {
          pkg = "dependent-sum";
          ver = "0.6.2.0";
          sha256 = "12k9wfl0i7g5mp9klh2720wz8rqxz4jl63zjzir9nxycb90qkxd5";
        } {};
      });
    });
  };

  pkgs = import sources.nixpkgs {
    config.allowBroken = true;
    # Yay, overlays!
    overlays = [overlay];
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
