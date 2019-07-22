module General.TestOpts where

import Data.Typeable (Typeable)
import Test.Tasty.Options (IsOption (..))

import           Data.Foldable         (foldl')
import           Data.Function         ((&))
import           System.Console.GetOpt

import           Servant.Client.Core   (BaseUrl, parseBaseUrl)

data Options = Options
  { _optionsExternalWD :: Maybe BaseUrl
  }
  deriving Show

defaultOpts :: Options
defaultOpts = Options Nothing

useExternalWD :: Maybe String -> Options -> Options
useExternalWD s o = o { _optionsExternalWD = s >>= parseBaseUrl }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['e'] ["external-wd"] (OptArg useExternalWD "URL") "use external webdriver program"
  ]

readArgs :: [String] -> IO Options
readArgs args =
  case getOpt Permute options args of
    (o, _, []) -> pure $ foldl' (&) defaultOpts o
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: -ehttp://url:port/path | --external-wd=http[s]://url:port/path to running webdriver program instance"

newtype OverrideWDUrl = OverrideWDUrl (Maybe BaseUrl)
  deriving Typeable

instance IsOption OverrideWDUrl where
  defaultValue = OverrideWDUrl Nothing
  parseValue = fmap (OverrideWDUrl . Just) . parseBaseUrl
  optionName = pure "existing-wd"
  optionHelp = pure "Provide the url of a running selenium/driver instance"
