{-# LANGUAGE OverloadedStrings #-}
module General.Webserver (testWebServer) where

import Network.Wai.Handler.Warp (defaultSettings, setPort)
import qualified Web.Scotty as W

testWebServer :: IO ()
testWebServer = W.scottyOpts (W.Options 0 $ setPort 9999 defaultSettings) $ do
  W.get "/" (W.file "test/test-webpage.html")
  W.get "/taco" (W.file "test/test-taco-webpage.html")
