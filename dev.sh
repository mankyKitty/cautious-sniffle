#! /usr/bin/env bash

# For speedy testing purposes, we don't bring up and tear down the
# selenium instance, using a separately managed long-running instance.
TEST_SELE_SERVER="http://localhost:4444/wd/hub"

ghcid -c 'ghci -ghci-script="driver.ghci"' --test=":main --existing-wd ${TEST_SELE_SERVER}" -W
