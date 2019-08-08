#!/usr/bin/env bash

S_VER="3.141"
S_VER_FULL="${S_VER}.59"

S_JAR_PATH="$HOME/selenium"
S_SCRIPT="/usr/local/bin/selenium-server"

if [ ! -d "$S_JAR_PATH" ];
then
  mkdir ~/selenium
fi

if [ ! -f "$S_SCRIPT" ];
then
  echo "java -jar $S_JAR_PATH/selenium-server-standalone-$S_VER_FULL.jar" > "$S_SCRIPT"
  chmod +x "$S_SCRIPT"
fi

echo "wget'ing selenium-standalone-server-$S_VER_FULL"
wget "http://selenium-release.storage.googleapis.com/$S_VER/selenium-server-standalone-$S_VER_FULL.jar" -P "$S_JAR_PATH/"

echo "apt-get'ing openjdk-8-jre"
sudo apt-get install -y openjdk-8-jre
