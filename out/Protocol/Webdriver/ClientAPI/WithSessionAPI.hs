{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Protocol.Webdriver.ClientAPI.WithSessionAPI
  ( WithSessionAPI (..)
  , WithSession
  , withSessionApi
  ) where

import qualified GHC.Generics                                        as GHC

import           Data.Proxy                                          (Proxy (..))
import           Data.Text                                           (Text)
import           Data.Vector                                         (Vector)
import           Protocol.Webdriver.ClientAPI.Types
import           Protocol.Webdriver.ClientAPI.Types.ElementId        (ElementId)
import           Protocol.Webdriver.ClientAPI.Types.Internal         (Value,
                                                                      WDJson)
import           Protocol.Webdriver.ClientAPI.Types.LocationStrategy (LocateUsing)
import           Protocol.Webdriver.ClientAPI.Types.Timeout          (Timeout)
import           Servant.API
import           Servant.API.ContentTypes.Waargonaut                 (WaargJSON)
import           Waargonaut.Types.Json                               (Json)

import           Servant.API.Generic

data WithSessionAPI route = WithSessionAPI
  { releaseActions          :: route :- "actions" :> Delete '[] NoContent
  , performActions          :: route :- "actions" :> ReqBody '[WaargJSON WDJson] PerformActions :> Post '[] NoContent
  , acceptAlert             :: route :- "alert" :> "accept" :> Post '[] NoContent
  , dismissAlert            :: route :- "alert" :> "dismiss" :> Post '[] NoContent
  , getAlertText            :: route :- "alert" :> "text" :> Get '[WaargJSON WDJson] Text
  , sendAlertText           :: route :- "alert" :> "text" :> ReqBody '[WaargJSON WDJson] SendAlertText :> Post '[] NoContent
  , back                    :: route :- "back" :> Post '[] NoContent
  , deleteCookie            :: route :- "cookie" :> Capture "name" Text :> Delete '[] NoContent
  , getNamedCookie          :: route :- "cookie" :> Capture "name" Text :> Get '[WaargJSON WDJson] Json
  , deleteAllCookies        :: route :- "cookie" :> Delete '[] NoContent
  , getAllCookies           :: route :- "cookie" :> Get '[WaargJSON WDJson] (Vector Json)
  , addCookie               :: route :- "cookie" :> ReqBody '[WaargJSON WDJson] AddCookie :> Post '[] NoContent
  , getActiveElement        :: route :- "element" :> "active" :> Post '[WaargJSON WDJson] (Value ElementId)
  , getElementAttribute     :: route :- "element" :> Capture "elementId" ElementId :> "attribute" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Value Text)
  , elementClear            :: route :- "element" :> Capture "elementId" ElementId :> "clear" :> Post '[] NoContent
  , elementClick            :: route :- "element" :> Capture "elementId" ElementId :> "click" :> Post '[] NoContent
  , getElementCSSValue      :: route :- "element" :> Capture "elementId" ElementId :> "css" :> Capture "propertyName" Text :> Get '[WaargJSON WDJson] (Value Text)
  , isElementDisplayed      :: route :- "element" :> Capture "elementId" ElementId :> "displayed" :> Get '[WaargJSON WDJson] (Value Bool)
  , findElementFromElement  :: route :- "element" :> Capture "elementId" ElementId :> "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value ElementId)
  , findElementsFromElement :: route :- "element" :> Capture "elementId" ElementId :> "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value (Vector ElementId))
  , isElementEnabled        :: route :- "element" :> Capture "elementId" ElementId :> "enabled" :> Get '[WaargJSON WDJson] (Value Bool)
  , getElementTagName       :: route :- "element" :> Capture "elementId" ElementId :> "name" :> Get '[WaargJSON WDJson] (Value Text)
  , getElementProperty      :: route :- "element" :> Capture "elementId" ElementId :> "property" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Value Text)
  , getElementRect          :: route :- "element" :> Capture "elementId" ElementId :> "rect" :> Get '[WaargJSON WDJson] (Value Json)
  , takeElementScreenshot   :: route :- "element" :> Capture "elementId" ElementId :> "screenshot" :> ReqBody '[WaargJSON WDJson] TakeElementScreenshot :> Get '[WaargJSON WDJson] (Value Text)
  , isElementSelected       :: route :- "element" :> Capture "elementId" ElementId :> "selected" :> Get '[WaargJSON WDJson] (Value Bool)
  , getElementText          :: route :- "element" :> Capture "elementId" ElementId :> "text" :> Get '[WaargJSON WDJson] (Value Text)
  , elementSendKeys         :: route :- "element" :> Capture "elementId" ElementId :> "value" :> ReqBody '[WaargJSON WDJson] ElementSendKeys :> Post '[] NoContent
  , findElement             :: route :- "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value ElementId)
  , findElements            :: route :- "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value (Vector ElementId))
  , executeAsyncScript      :: route :- "execute" :> "async" :> ReqBody '[WaargJSON WDJson] ExecuteAsyncScript :> Post '[WaargJSON WDJson] Json
  , executeScript           :: route :- "execute" :> "sync" :> ReqBody '[WaargJSON WDJson] ExecuteScript :> Post '[WaargJSON WDJson] Json
  , forward                 :: route :- "forward" :> Post '[] NoContent
  , switchToParentFrame     :: route :- "frame" :> "parent" :> Post '[] NoContent
  , switchToFrame           :: route :- "frame" :> ReqBody '[WaargJSON WDJson] SwitchToFrame :> Post '[] NoContent
  , refresh                 :: route :- "refresh" :> Post '[] NoContent
  , takeScreenshot          :: route :- "screenshot" :> Get '[WaargJSON WDJson] Text
  , getPageSource           :: route :- "source" :> Get '[] NoContent
  , getTimeouts             :: route :- "timeouts" :> Get '[WaargJSON WDJson] Json
  , setTimeouts             :: route :- "timeouts" :> ReqBody '[WaargJSON WDJson] Timeout :> Post '[] NoContent
  , getTitle                :: route :- "title" :> Get '[WaargJSON WDJson] Text
  , getUrl                  :: route :- "url" :> Get '[WaargJSON WDJson] (Value WDUri)
  , navigateTo              :: route :- "url" :> ReqBody '[WaargJSON WDJson] WDUri :> Post '[WaargJSON WDJson] (Value ())
  , fullscreenWindow        :: route :- "window" :> "fullscreen" :> Post '[WaargJSON WDJson] Json
  , getWindowHandles        :: route :- "window" :> "handles" :> Get '[WaargJSON WDJson] (Vector Text)
  , maximizeWindow          :: route :- "window" :> "maximize" :> Post '[WaargJSON WDJson] Json
  , minimizeWindow          :: route :- "window" :> "minimize" :> Post '[WaargJSON WDJson] Json
  , createWindow            :: route :- "window" :> "new" :> ReqBody '[WaargJSON WDJson] CreateWindow :> Post '[WaargJSON WDJson] (Value NewWindow)
  , getWindowRect           :: route :- "window" :> "rect" :> Get '[WaargJSON WDJson] Json
  , setWindowRect           :: route :- "window" :> "rect" :> ReqBody '[WaargJSON WDJson] SetWindowRect :> Post '[WaargJSON WDJson] Json
  , closeWindow             :: route :- "window" :> Delete '[] NoContent
  , getWindowHandle         :: route :- "window" :> Get '[WaargJSON WDJson] (Value WindowHandle)
  , switchToWindow          :: route :- "window" :> ReqBody '[WaargJSON WDJson] SwitchToWindow :> Post '[] NoContent
  , deleteSession           :: route :- Delete '[] NoContent
  }
  deriving GHC.Generic

type WithSession =
  ToServantApi WithSessionAPI

withSessionApi :: Proxy WithSession
withSessionApi = Proxy
