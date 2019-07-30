{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Protocol.Webdriver.ClientAPI.SessionAPI
  ( SessionAPI (..)
  , ElementAPI (..)
  , WindowAPI (..)
  , elemApi
  , sessionApi
  , windowApi
  ) where

import qualified GHC.Generics                        as GHC

import           Data.Proxy                          (Proxy (..))

import           Data.ByteString                     (ByteString)
import           Data.Text                           (Text)
import           Data.Vector                         (Vector)

import           Servant.API
import           Servant.API.ContentTypes.Waargonaut (WaargJSON)

import           Waargonaut.Types.Json               (Json)

import           Servant.API.Generic

import           Protocol.Webdriver.ClientAPI.Types

import           Servant.API.Client.HollowBody              (HollowBody)

data ElementAPI route = ElementAPI
  { getElementAttribute     :: route :- "attribute" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Success ByteString)
  , elementClear            :: route :- "clear" :> HollowBody '[WaargJSON WDJson] :> Post '[WaargJSON WDJson] (Success ())
  , elementClick            :: route :- "click" :> HollowBody '[WaargJSON WDJson] :> Post '[WaargJSON WDJson] (Success ())
  , getElementCSSValue      :: route :- "css" :> Capture "propertyName" Text :> Get '[WaargJSON WDJson] (Success PropertyVal)
  , isElementDisplayed      :: route :- "displayed" :> Get '[WaargJSON WDJson] (Success Bool)
  , findElementFromElement  :: route :- "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Success ElementId)
  , findElementsFromElement :: route :- "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Success (Vector ElementId))
  , isElementEnabled        :: route :- "enabled" :> Get '[WaargJSON WDJson] (Success Bool)
  , getElementTagName       :: route :- "name" :> Get '[WaargJSON WDJson] (Success Text)
  , getElementProperty      :: route :- "property" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Success PropertyVal)
  , getElementRect          :: route :- "rect" :> Get '[WaargJSON WDJson] (Success WDRect)
  , takeElementScreenshot   :: route :- "screenshot" :> ReqBody '[WaargJSON WDJson] TakeElementScreenshot :> Get '[WaargJSON WDJson] (Success Base64)
  , isElementSelected       :: route :- "selected" :> Get '[WaargJSON WDJson] (Success Bool)
  , getElementText          :: route :- "text" :> Get '[WaargJSON WDJson] (Success Text)
  , elementSendKeys         :: route :- "value" :> ReqBody '[WaargJSON WDJson] ElementSendKeys :> Post '[WaargJSON WDJson] (Success ())
  }
  deriving GHC.Generic

elemApi :: Proxy (ToServantApi ElementAPI)
elemApi = genericApi (Proxy @ElementAPI)

data WindowAPI route = WindowAPI
  { fullscreenWindow        :: route :- "fullscreen" :> Post '[WaargJSON WDJson] (Success WDRect)
  , getWindowHandles        :: route :- "handles" :> Get '[WaargJSON WDJson] (Vector WindowHandle)
  , maximizeWindow          :: route :- "maximize" :> Post '[WaargJSON WDJson] (Success WDRect)
  , minimizeWindow          :: route :- "minimize" :> Post '[WaargJSON WDJson] (Success WDRect)
  , createWindow            :: route :- "new" :> ReqBody '[WaargJSON WDJson] CreateWindow :> Post '[WaargJSON WDJson] (Success NewWindow)
  , getWindowRect           :: route :- "rect" :> Get '[WaargJSON WDJson] (Success WDRect)
  , setWindowRect           :: route :- "rect" :> ReqBody '[WaargJSON WDJson] WDRect :> Post '[WaargJSON WDJson] (Success WDRect)
  , closeWindow             :: route :- Delete '[WaargJSON WDJson] (Success (Vector WindowHandle))
  , getWindowHandle         :: route :- Get '[WaargJSON WDJson] (Success WindowHandle)
  , switchToWindow          :: route :- ReqBody '[WaargJSON WDJson] SwitchToWindow :> Post '[WaargJSON WDJson] (Success ())
  }
  deriving GHC.Generic

windowApi :: Proxy (ToServantApi WindowAPI)
windowApi = genericApi (Proxy @WindowAPI)

data SessionAPI route = SessionAPI
  { releaseActions          :: route :- "actions" :> Delete '[WaargJSON WDJson] (Success ())
  , performActions          :: route :- "actions" :> ReqBody '[WaargJSON WDJson] PerformActions :> Post '[WaargJSON WDJson] (Success ())
  , acceptAlert             :: route :- "alert" :> "accept" :> HollowBody '[WaargJSON WDJson] :> Post '[WaargJSON WDJson] (Success ())
  , dismissAlert            :: route :- "alert" :> "dismiss" :> HollowBody '[WaargJSON WDJson] :> Post '[WaargJSON WDJson] (Success ())
  , getAlertText            :: route :- "alert" :> "text" :> Get '[WaargJSON WDJson] (Success Text)
  , sendAlertText           :: route :- "alert" :> "text" :> ReqBody '[WaargJSON WDJson] SendAlertText :> Post '[WaargJSON WDJson] (Success ())
  , back                    :: route :- "back" :> Post '[WaargJSON WDJson] (Success ())
  , deleteCookie            :: route :- "cookie" :> Capture "name" Text :> Delete '[WaargJSON WDJson] (Success ())
  , getNamedCookie          :: route :- "cookie" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Success Cookie)
  , deleteAllCookies        :: route :- "cookie" :> Delete '[WaargJSON WDJson] (Success ())
  , getAllCookies           :: route :- "cookie" :> Get '[WaargJSON WDJson] (Success (Vector Cookie))
  , addCookie               :: route :- "cookie" :> ReqBody '[WaargJSON WDJson] Cookie :> Post '[WaargJSON WDJson] (Success ())
  , getActiveElement        :: route :- "element" :> "active" :> Post '[WaargJSON WDJson] (Success ElementId)
  , withElement             :: route :- "element" :> Capture "elementId" ElementId :> ToServantApi ElementAPI
  , findElement             :: route :- "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Success ElementId)
  , findElements            :: route :- "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Success (Vector ElementId))
  , executeAsyncScript      :: route :- "execute" :> "async" :> ReqBody '[WaargJSON WDJson] ExecuteAsyncScript :> Post '[WaargJSON WDJson] (Success Json)
  , executeScript           :: route :- "execute" :> "sync" :> ReqBody '[WaargJSON WDJson] ExecuteScript :> Post '[WaargJSON WDJson] (Success Json)
  , forward                 :: route :- "forward" :> Post '[WaargJSON WDJson] (Success ())
  , switchToParentFrame     :: route :- "frame" :> "parent" :> Post '[WaargJSON WDJson] (Success ())
  , switchToFrame           :: route :- "frame" :> ReqBody '[WaargJSON WDJson] SwitchToFrame :> Post '[WaargJSON WDJson] (Success ())
  , refresh                 :: route :- "refresh" :> HollowBody '[WaargJSON WDJson] :> Post '[WaargJSON WDJson] (Success ())
  , takeScreenshot          :: route :- "screenshot" :> Get '[WaargJSON WDJson] (Success Base64)
  , getPageSource           :: route :- "source" :> Get '[WaargJSON WDJson] (Success Text)
  , getTimeouts             :: route :- "timeouts" :> Get '[WaargJSON WDJson] (Success Timeout)
  , setTimeouts             :: route :- "timeouts" :> ReqBody '[WaargJSON WDJson] Timeout :> Post '[WaargJSON WDJson] (Success ())
  , getTitle                :: route :- "title" :> Get '[WaargJSON WDJson] (Success Text)
  , getUrl                  :: route :- "url" :> Get '[WaargJSON WDJson] (Success WDUri)
  , navigateTo              :: route :- "url" :> ReqBody '[WaargJSON WDJson] WDUri :> Post '[WaargJSON WDJson] (Success ())
  , withWindow              :: route :- "window" :> ToServantApi WindowAPI
  , deleteSession           :: route :- Delete '[WaargJSON WDJson] (Success ())
  }
  deriving GHC.Generic

sessionApi :: Proxy (ToServantApi SessionAPI)
sessionApi = genericApi (Proxy @SessionAPI)
