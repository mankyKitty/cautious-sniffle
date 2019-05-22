{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
module Protocol.Webdriver.ClientAPI.SessionAPI
  ( SessionAPI (..)
  , ElementAPI (..)
  , WindowAPI (..)
  , elemApi
  , sessionApi
  , windowApi
  ) where

import qualified GHC.Generics                                        as GHC

import           Data.Proxy                                          (Proxy (..))
import           Data.Text                                           (Text)
import           Data.Vector                                         (Vector)
import           Protocol.Webdriver.ClientAPI.Types
import           Protocol.Webdriver.ClientAPI.Types.ElementId        (ElementId)
import           Protocol.Webdriver.ClientAPI.Types.Internal         (Value,Base64,
                                                                      WDJson)
import           Protocol.Webdriver.ClientAPI.Types.LocationStrategy (LocateUsing)
import           Protocol.Webdriver.ClientAPI.Types.Timeout          (Timeout)
import           Servant.API
import           Servant.API.ContentTypes.Waargonaut                 (WaargJSON)
import           Waargonaut.Types.Json                               (Json)

import           Servant.API.Generic

data ElementAPI route = ElementAPI
  { getElementAttribute     :: route :- "attribute" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Value Text)
  , elementClear            :: route :- "clear" :> Post '[] NoContent
  , elementClick            :: route :- "click" :> Post '[] NoContent
  , getElementCSSValue      :: route :- "css" :> Capture "propertyName" Text :> Get '[WaargJSON WDJson] (Value Text)
  , isElementDisplayed      :: route :- "displayed" :> Get '[WaargJSON WDJson] (Value Bool)
  , findElementFromElement  :: route :- "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value ElementId)
  , findElementsFromElement :: route :- "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value (Vector ElementId))
  , isElementEnabled        :: route :- "enabled" :> Get '[WaargJSON WDJson] (Value Bool)
  , getElementTagName       :: route :- "name" :> Get '[WaargJSON WDJson] (Value Text)
  , getElementProperty      :: route :- "property" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Value Text)
  , getElementRect          :: route :- "rect" :> Get '[WaargJSON WDJson] (Value Json)
  , takeElementScreenshot   :: route :- "screenshot" :> ReqBody '[WaargJSON WDJson] TakeElementScreenshot :> Get '[WaargJSON WDJson] (Value Text)
  , isElementSelected       :: route :- "selected" :> Get '[WaargJSON WDJson] (Value Bool)
  , getElementText          :: route :- "text" :> Get '[WaargJSON WDJson] (Value Text)
  , elementSendKeys         :: route :- "value" :> ReqBody '[WaargJSON WDJson] ElementSendKeys :> Post '[] NoContent
  }
  deriving GHC.Generic

elemApi :: Proxy (ToServantApi ElementAPI)
elemApi = genericApi (Proxy @ElementAPI)

data WindowAPI route = WindowAPI
  { fullscreenWindow        :: route :- "fullscreen" :> Post '[WaargJSON WDJson] Json
  , getWindowHandles        :: route :- "handles" :> Get '[WaargJSON WDJson] (Vector Text)
  , maximizeWindow          :: route :- "maximize" :> Post '[WaargJSON WDJson] Json
  , minimizeWindow          :: route :- "minimize" :> Post '[WaargJSON WDJson] Json
  , createWindow            :: route :- "new" :> ReqBody '[WaargJSON WDJson] CreateWindow :> Post '[WaargJSON WDJson] (Value NewWindow)
  , getWindowRect           :: route :- "rect" :> Get '[WaargJSON WDJson] Json
  , setWindowRect           :: route :- "rect" :> ReqBody '[WaargJSON WDJson] SetWindowRect :> Post '[WaargJSON WDJson] Json
  , closeWindow             :: route :- Delete '[] NoContent
  , getWindowHandle         :: route :- Get '[WaargJSON WDJson] (Value WindowHandle)
  , switchToWindow          :: route :- ReqBody '[WaargJSON WDJson] SwitchToWindow :> Post '[] NoContent
  }
  deriving GHC.Generic

windowApi :: Proxy (ToServantApi WindowAPI)
windowApi = genericApi (Proxy @WindowAPI)

data SessionAPI route = SessionAPI
  { releaseActions          :: route :- "actions" :> Delete '[] NoContent
  , performActions          :: route :- "actions" :> ReqBody '[WaargJSON WDJson] PerformActions :> Post '[] NoContent
  , acceptAlert             :: route :- "alert" :> "accept" :> Post '[] NoContent
  , dismissAlert            :: route :- "alert" :> "dismiss" :> Post '[] NoContent
  , getAlertText            :: route :- "alert" :> "text" :> Get '[WaargJSON WDJson] (Value Text)
  , sendAlertText           :: route :- "alert" :> "text" :> ReqBody '[WaargJSON WDJson] SendAlertText :> Post '[] NoContent
  , back                    :: route :- "back" :> Post '[] NoContent
  , deleteCookie            :: route :- "cookie" :> Capture "name" Text :> Delete '[] NoContent
  , getNamedCookie          :: route :- "cookie" :> Capture "name" Text :> Get '[WaargJSON WDJson] Json
  , deleteAllCookies        :: route :- "cookie" :> Delete '[] NoContent
  , getAllCookies           :: route :- "cookie" :> Get '[WaargJSON WDJson] (Vector Json)
  , addCookie               :: route :- "cookie" :> ReqBody '[WaargJSON WDJson] AddCookie :> Post '[] NoContent
  , getActiveElement        :: route :- "element" :> "active" :> Post '[WaargJSON WDJson] (Value ElementId)
  , withElement             :: route :- "element" :> Capture "elementId" ElementId :> ToServantApi ElementAPI
  , findElement             :: route :- "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value ElementId)
  , findElements            :: route :- "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value (Vector ElementId))
  , executeAsyncScript      :: route :- "execute" :> "async" :> ReqBody '[WaargJSON WDJson] ExecuteAsyncScript :> Post '[WaargJSON WDJson] Json
  , executeScript           :: route :- "execute" :> "sync" :> ReqBody '[WaargJSON WDJson] ExecuteScript :> Post '[WaargJSON WDJson] Json
  , forward                 :: route :- "forward" :> Post '[] NoContent
  , switchToParentFrame     :: route :- "frame" :> "parent" :> Post '[] NoContent
  , switchToFrame           :: route :- "frame" :> ReqBody '[WaargJSON WDJson] SwitchToFrame :> Post '[] NoContent
  , refresh                 :: route :- "refresh" :> Post '[] NoContent
  , takeScreenshot          :: route :- "screenshot" :> Get '[WaargJSON WDJson] (Value Base64)
  , getPageSource           :: route :- "source" :> Get '[] NoContent
  , getTimeouts             :: route :- "timeouts" :> Get '[WaargJSON WDJson] (Value Timeout)
  , setTimeouts             :: route :- "timeouts" :> ReqBody '[WaargJSON WDJson] Timeout :> Post '[] NoContent
  , getTitle                :: route :- "title" :> Get '[WaargJSON WDJson] (Value Text)
  , getUrl                  :: route :- "url" :> Get '[WaargJSON WDJson] (Value WDUri)
  , navigateTo              :: route :- "url" :> ReqBody '[WaargJSON WDJson] WDUri :> Post '[WaargJSON WDJson] (Value ())
  , withWindow              :: route :- "window" :> ToServantApi WindowAPI
  , deleteSession           :: route :- Delete '[] NoContent
  }
  deriving GHC.Generic

sessionApi :: Proxy (ToServantApi SessionAPI)
sessionApi = genericApi (Proxy @SessionAPI)
