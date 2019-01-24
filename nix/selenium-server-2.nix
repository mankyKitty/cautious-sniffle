{ nixpkgs ? import ./nixpkgs.nix
}:
let
  pkgs = import nixpkgs {};

  release = "2.53";
  patch_version = "1";

  ver = "${release}.${patch_version}";

  binName = "selenium-server-standalone-${ver}";
  jarName = "${binName}.jar";
in
  pkgs.stdenv.mkDerivation rec {
    name = "selenium-server-${ver}";
    version = ver;

    src = pkgs.fetchurl {
      url = "https://selenium-release.storage.googleapis.com/${release}/${jarName}";
      sha256 = "1y3w1c2173vn2yqy6047l6lxmg919xyi19ccw4my7cm5bhx6vkhw";
    };

    buildInputs = [ pkgs.makeWrapper pkgs.openjdk ];

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/share/java
      mkdir -p $out/bin

      cp ${src} $out/share/java/${jarName}

      makeWrapper ${pkgs.jre}/bin/java $out/bin/${binName} \
        --add-flags "-jar $out/share/java/${jarName}"
    '';

  }
