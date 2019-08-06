module General.TestOpts
  ( OverrideWDUrl (..)
  ) where

import           Test.Tasty.Options  (IsOption (..))

import           Servant.Client.Core (BaseUrl, parseBaseUrl)

newtype OverrideWDUrl = OverrideWDUrl (Maybe BaseUrl)

instance IsOption OverrideWDUrl where
  defaultValue = OverrideWDUrl Nothing
  parseValue = fmap (OverrideWDUrl . Just) . parseBaseUrl
  optionName = pure "existing-wd"
  optionHelp = pure "Provide the url of a running selenium/driver instance"
