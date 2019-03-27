{ stdenv, fetchurl, makeWrapper, jre
, htmlunit-driver, chromedriver, chromeSupport ? true }:

with stdenv.lib;

let
  minorVersion = "3.141";
  patchVersion = "59";

in stdenv.mkDerivation rec {
  name = "selenium-server-standalone-${version}";
  version = "${minorVersion}.${patchVersion}";

  src = fetchurl {
    url = "http://selenium-release.storage.googleapis.com/${minorVersion}/selenium-server-standalone-${version}.jar";
    sha256 = "acf71b77d1b66b55db6fb0bed6d8bae2bbd481311bcbedfeff472c0d15e8f3cb";
  };

  unpackPhase = "true";

  buildInputs = [ jre makeWrapper chromedriver ];

  installPhase = ''
    mkdir -p $out/share/lib/${name}
    cp $src $out/share/lib/${name}/${name}.jar
    makeWrapper ${jre}/bin/java $out/bin/selenium-server \
      --add-flags "-cp $out/share/lib/${name}/${name}.jar:${htmlunit-driver}/share/lib/${htmlunit-driver.name}/${htmlunit-driver.name}.jar" \
      --add-flags -Dwebdriver.chrome.driver=${chromedriver}/bin/chromedriver \
      --add-flags "org.openqa.grid.selenium.GridLauncherV3"
  '';

  meta = {
    homepage = http://www.seleniumhq.org/;
    description = "Selenium Server for remote WebDriver";
    maintainers = with maintainers; [ coconnor offline ];
    platforms = stdenv.lib.platforms.all;
    license = licenses.asl20;
  };
}
