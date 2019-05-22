{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -Wno-missing-signatures#-}
module Protocol.Webdriver.ClientAPI.Record
  ( -- * Types
    WebDriverAPI
  , WDApi (..)
  , InSession (..)
    -- * Constructor
  , mkWDApi
    -- * API Proxy
  , webdriverApi
  ) where

import           Data.Proxy                                          (Proxy (..))
import           Data.Text                                           (Text)
import           Data.Vector                                         (Vector)
import           Protocol.Webdriver.ClientAPI.Types
import           Protocol.Webdriver.ClientAPI.Types.ElementId        (ElementId)
import           Protocol.Webdriver.ClientAPI.Types.Internal         (Value,
                                                                      WDJson)
import           Protocol.Webdriver.ClientAPI.Types.LocationStrategy (LocateUsing)
import           Protocol.Webdriver.ClientAPI.Types.Session          (NewSession,
                                                                      Session,
                                                                      SessionId)
import           Protocol.Webdriver.ClientAPI.Types.Timeout          (Timeout)
import           Servant.API
import           Servant.API.ContentTypes.Waargonaut                 (WaargJSON)
import           Servant.Client                                      (ClientM)
import qualified Servant.Client                                      as C
import           Waargonaut.Types.Json                               (Json)

type WebDriverAPI
  = ("status" :> Get '[WaargJSON WDJson] Json)
  :<|> "session" :> (
         (ReqBody '[WaargJSON WDJson] NewSession :> Post '[WaargJSON WDJson] (Value Session))
    :<|> Capture "sessionId" SessionId :> WithSessionAPI
  )

type WithSessionAPI
  =    ("actions" :> Delete '[] NoContent)
  :<|> ("actions" :> ReqBody '[WaargJSON WDJson] PerformActions :> Post '[] NoContent)

  :<|> ("alert" :> "accept" :> Post '[] NoContent)
  :<|> ("alert" :> "dismiss" :> Post '[] NoContent)
  :<|> ("alert" :> "text" :> Get '[WaargJSON WDJson] Text)
  :<|> ("alert" :> "text" :> ReqBody '[WaargJSON WDJson] SendAlertText :> Post '[] NoContent)

  :<|> ("back" :> Post '[] NoContent)

  :<|> ("cookie" :> Capture "name" Text :> Delete '[] NoContent)
  :<|> ("cookie" :> Capture "name" Text :> Get '[WaargJSON WDJson] Json)
  :<|> ("cookie" :> Delete '[] NoContent)
  :<|> ("cookie" :> Get '[WaargJSON WDJson] (Vector Json))
  :<|> ("cookie" :> ReqBody '[WaargJSON WDJson] AddCookie :> Post '[] NoContent)

  :<|> ("element" :> "active" :> Post '[WaargJSON WDJson] (Value ElementId))
  :<|> ("element" :> Capture "elementId" ElementId :> "attribute" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Value Text))
  :<|> ("element" :> Capture "elementId" ElementId :> "clear" :> Post '[] NoContent)
  :<|> ("element" :> Capture "elementId" ElementId :> "click" :> Post '[] NoContent)
  :<|> ("element" :> Capture "elementId" ElementId :> "css" :> Capture "propertyName" Text :> Get '[WaargJSON WDJson] (Value Text))
  :<|> ("element" :> Capture "elementId" ElementId :> "displayed" :> Get '[WaargJSON WDJson] (Value Bool))
  :<|> ("element" :> Capture "elementId" ElementId :> "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value ElementId))
  :<|> ("element" :> Capture "elementId" ElementId :> "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value (Vector ElementId)))
  :<|> ("element" :> Capture "elementId" ElementId :> "enabled" :> Get '[WaargJSON WDJson] (Value Bool))
  :<|> ("element" :> Capture "elementId" ElementId :> "name" :> Get '[WaargJSON WDJson] (Value Text))
  :<|> ("element" :> Capture "elementId" ElementId :> "property" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Value Text))
  :<|> ("element" :> Capture "elementId" ElementId :> "rect" :> Get '[WaargJSON WDJson] (Value Json))
  :<|> ("element" :> Capture "elementId" ElementId :> "screenshot" :> ReqBody '[WaargJSON WDJson] TakeElementScreenshot :> Get '[WaargJSON WDJson] (Value Text))
  :<|> ("element" :> Capture "elementId" ElementId :> "selected" :> Get '[WaargJSON WDJson] (Value Bool))
  :<|> ("element" :> Capture "elementId" ElementId :> "text" :> Get '[WaargJSON WDJson] (Value Text))
  :<|> ("element" :> Capture "elementId" ElementId :> "value" :> ReqBody '[WaargJSON WDJson] ElementSendKeys :> Post '[] NoContent)

  :<|> ("element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value ElementId))
  :<|> ("elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value (Vector ElementId)))

  :<|> ("execute" :> "async" :> ReqBody '[WaargJSON WDJson] ExecuteAsyncScript :> Post '[WaargJSON WDJson] Json)
  :<|> ("execute" :> "sync" :> ReqBody '[WaargJSON WDJson] ExecuteScript :> Post '[WaargJSON WDJson] Json)

  :<|> ("forward" :> Post '[] NoContent)

  :<|> ("frame" :> "parent" :> Post '[] NoContent)
  :<|> ("frame" :> ReqBody '[WaargJSON WDJson] SwitchToFrame :> Post '[] NoContent)

  :<|> ("refresh" :> Post '[] NoContent)

  :<|> ("screenshot" :> Get '[WaargJSON WDJson] Text)

  :<|> ("source" :> Get '[] NoContent)

  :<|> ("timeouts" :> Get '[WaargJSON WDJson] Json)
  :<|> ("timeouts" :> ReqBody '[WaargJSON WDJson] Timeout :> Post '[] NoContent)

  :<|> ("title" :> Get '[WaargJSON WDJson] Text)

  :<|> ("url" :> Get '[WaargJSON WDJson] (Value WDUri))
  :<|> ("url" :> ReqBody '[WaargJSON WDJson] WDUri :> Post '[WaargJSON WDJson] (Value ()))

  :<|> ("window" :> "fullscreen" :> Post '[WaargJSON WDJson] Json)
  :<|> ("window" :> "handles" :> Get '[WaargJSON WDJson] (Vector Text))
  :<|> ("window" :> "maximize" :> Post '[WaargJSON WDJson] Json)
  :<|> ("window" :> "minimize" :> Post '[WaargJSON WDJson] Json)
  :<|> ("window" :> "new" :> ReqBody '[WaargJSON WDJson] CreateWindow :> Post '[WaargJSON WDJson] (Value NewWindow))
  :<|> ("window" :> "rect" :> Get '[WaargJSON WDJson] Json)
  :<|> ("window" :> "rect" :> ReqBody '[WaargJSON WDJson] SetWindowRect :> Post '[WaargJSON WDJson] Json)
  :<|> ("window" :> Delete '[] NoContent)
  :<|> ("window" :> Get '[WaargJSON WDJson] (Value WindowHandle))
  :<|> ("window" :> ReqBody '[WaargJSON WDJson] SwitchToWindow :> Post '[] NoContent)

  :<|> (Delete '[] NoContent)

webdriverApi :: Proxy WebDriverAPI
webdriverApi = Proxy

data InSession = InSession
  { releaseActions          :: ClientM NoContent
  , performActions          :: PerformActions -> ClientM NoContent
  , acceptAlert             :: ClientM NoContent
  , dismissAlert            :: ClientM NoContent
  , getAlertText            :: ClientM Text
  , sendAlertText           :: SendAlertText -> ClientM NoContent
  , back                    :: ClientM NoContent
  , deleteCookie            :: Text -> ClientM NoContent
  , getNamedCookie          :: Text -> ClientM Json
  , deleteAllCookies        :: ClientM NoContent
  , getAllCookies           :: ClientM (Vector Json)
  , addCookie               :: AddCookie -> ClientM NoContent
  , getActiveElement        :: ClientM (Value ElementId)
  , getElementAttribute     :: ElementId -> Text -> ClientM (Value Text)
  , elementClear            :: ElementId -> ClientM NoContent
  , elementClick            :: ElementId -> ClientM NoContent
  , getElementCSSValue      :: ElementId -> Text -> ClientM (Value Text)
  , isElementDisplayed      :: ElementId -> ClientM (Value Bool)
  , findElementFromElement  :: ElementId -> LocateUsing -> ClientM (Value ElementId)
  , findElementsFromElement :: ElementId -> LocateUsing -> ClientM (Value (Vector ElementId))
  , isElementEnabled        :: ElementId -> ClientM (Value Bool)
  , getElementTagName       :: ElementId -> ClientM (Value Text)
  , getElementProperty      :: ElementId -> Text -> ClientM (Value Text)
  , getElementRect          :: ElementId -> ClientM (Value Json)
  , takeElementScreenshot   :: ElementId -> TakeElementScreenshot -> ClientM (Value Text)
  , isElementSelected       :: ElementId -> ClientM (Value Bool)
  , getElementText          :: ElementId -> ClientM (Value Text)
  , elementSendKeys         :: ElementId -> ElementSendKeys -> ClientM NoContent
  , findElement             :: LocateUsing -> ClientM (Value ElementId)
  , findElements            :: LocateUsing -> ClientM (Value (Vector ElementId))
  , executeAsyncScript      :: ExecuteAsyncScript -> ClientM Json
  , executeScript           :: ExecuteScript -> ClientM Json
  , forward                 :: ClientM NoContent
  , switchToParentFrame     :: ClientM NoContent
  , switchToFrame           :: SwitchToFrame -> ClientM NoContent
  , refresh                 :: ClientM NoContent
  , takeScreenshot          :: ClientM Text
  , getPageSource           :: ClientM NoContent
  , getTimeouts             :: ClientM Json
  , setTimeouts             :: Timeout -> ClientM NoContent
  , getTitle                :: ClientM Text
  , getUrl                  :: ClientM (Value WDUri)
  , navigateTo              :: WDUri -> ClientM (Value ())
  , fullscreenWindow        :: ClientM Json
  , getWindowHandles        :: ClientM (Vector Text)
  , maximizeWindow          :: ClientM Json
  , minimizeWindow          :: ClientM Json
  , createWindow            :: CreateWindow -> ClientM (Value NewWindow)
  , getWindowRect           :: ClientM Json
  , setWindowRect           :: SetWindowRect -> ClientM Json
  , closeWindow             :: ClientM NoContent
  , getWindowHandle         :: ClientM (Value WindowHandle)
  , switchToWindow          :: SwitchToWindow -> ClientM NoContent
  , deleteSession           :: ClientM NoContent
  }

data WDApi = WDApi
  { status      :: ClientM Json
  , newSession  :: NewSession -> ClientM (Value Session)
  , withSession :: SessionId -> InSession
  }

mkWDApi = WDApi {..}
  where
    status :<|> newSession :<|> needsSessionId =
      C.client webdriverApi

    withSession sessId = InSession {..}
      where
        releaseActions
          :<|> performActions
          :<|> acceptAlert
          :<|> dismissAlert
          :<|> getAlertText
          :<|> sendAlertText
          :<|> back
          :<|> deleteCookie
          :<|> getNamedCookie
          :<|> deleteAllCookies
          :<|> getAllCookies
          :<|> addCookie
          :<|> getActiveElement
          :<|> getElementAttribute
          :<|> elementClear
          :<|> elementClick
          :<|> getElementCSSValue
          :<|> isElementDisplayed
          :<|> findElementFromElement
          :<|> findElementsFromElement
          :<|> isElementEnabled
          :<|> getElementTagName
          :<|> getElementProperty
          :<|> getElementRect
          :<|> takeElementScreenshot
          :<|> isElementSelected
          :<|> getElementText
          :<|> elementSendKeys
          :<|> findElement
          :<|> findElements
          :<|> executeAsyncScript
          :<|> executeScript
          :<|> forward
          :<|> switchToParentFrame
          :<|> switchToFrame
          :<|> refresh
          :<|> takeScreenshot
          :<|> getPageSource
          :<|> getTimeouts
          :<|> setTimeouts
          :<|> getTitle
          :<|> getUrl
          :<|> navigateTo
          :<|> fullscreenWindow
          :<|> getWindowHandles
          :<|> maximizeWindow
          :<|> minimizeWindow
          :<|> createWindow
          :<|> getWindowRect
          :<|> setWindowRect
          :<|> closeWindow
          :<|> getWindowHandle
          :<|> switchToWindow
          :<|> deleteSession = needsSessionId sessId
