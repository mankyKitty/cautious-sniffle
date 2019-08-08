#!/usr/bin/env bash

G_VER="v0.24.0"
G_PATH="/usr/local/bin"

echo "wget'ing geckodriver"
wget "https://github.com/mozilla/geckodriver/releases/download/$G_VER/geckodriver-${G_VER}-linux64.tar.gz" -P "/tmp/"
cd /tmp
tar xvf "geckodriver-${G_VER}-linux64.tar.gz"
mv geckodriver /usr/local/bin/geckodriver
