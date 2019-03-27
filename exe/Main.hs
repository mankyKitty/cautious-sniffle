module Main where

import System.Directory (createDirectoryIfMissing)

import Protocol.Webdriver.Generate (createFiles)

outRoot = "generated/Protocol/Webdriver"
outTree = outRoot <> "/ClientAPI"

main :: IO ()
main = do
  createDirectoryIfMissing True outTree
  createFiles "protocol/webdriver.json" outRoot
