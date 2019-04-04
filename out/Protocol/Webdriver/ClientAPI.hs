{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures#-}
module Protocol.Webdriver.ClientAPI
  ( -- * API Type
    WebDriverAPI
    -- * API Proxy
  , webdriverApi
    -- * API Actions
  , releaseActions, performActions, acceptAlert, dismissAlert,
    getAlertText, sendAlertText, back, deleteCookie, getNamedCookie,
    deleteAllCookies, getAllCookies, addCookie, getActiveElement,
    getElementAttribute, elementClear, elementClick,
    getElementCSSValue, isElementDisplayed, findElementFromElement,
    findElementsFromElement, isElementEnabled, getElementTagName,
    getElementProperty, getElementRect, takeElementScreenshot,
    isElementSelected, getElementText, elementSendKeys, findElement,
    findElements, executeAsyncScript, executeScript, forward,
    switchToParentFrame, switchToFrame, refresh, takeScreenshot,
    getPageSource, getTimeouts, setTimeouts, getTitle, getUrl,
    navigateTo, fullscreenWindow, getWindowHandles, maximizeWindow,
    minimizeWindow, createWindow, getWindowRect, setWindowRect,
    closeWindow, getWindowHandle, switchToWindow, deleteSession,
    newSession, status
    -- * Helpers
  , defaultWebdriverClient
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
import           Servant.Client
import           Waargonaut.Types.Json                               (Json)

defaultWebdriverClient :: BaseUrl
defaultWebdriverClient = BaseUrl Http "localhost" 4444 "/wd/hub"

type WebDriverAPI =
       ("session" :> Capture "sessionId" SessionId :> "actions" :> Delete '[] NoContent)
  :<|> ("session" :> Capture "sessionId" SessionId :> "actions" :> ReqBody '[WaargJSON WDJson] PerformActions :> Post '[] NoContent)

  :<|> ("session" :> Capture "sessionId" SessionId :> "alert" :> "accept" :> Post '[] NoContent)
  :<|> ("session" :> Capture "sessionId" SessionId :> "alert" :> "dismiss" :> Post '[] NoContent)
  :<|> ("session" :> Capture "sessionId" SessionId :> "alert" :> "text" :> Get '[WaargJSON WDJson] Text)
  :<|> ("session" :> Capture "sessionId" SessionId :> "alert" :> "text" :> ReqBody '[WaargJSON WDJson] SendAlertText :> Post '[] NoContent)

  :<|> ("session" :> Capture "sessionId" SessionId :> "back" :> Post '[] NoContent)

  :<|> ("session" :> Capture "sessionId" SessionId :> "cookie" :> Capture "name" Text :> Delete '[] NoContent)
  :<|> ("session" :> Capture "sessionId" SessionId :> "cookie" :> Capture "name" Text :> Get '[WaargJSON WDJson] Json)
  :<|> ("session" :> Capture "sessionId" SessionId :> "cookie" :> Delete '[] NoContent)
  :<|> ("session" :> Capture "sessionId" SessionId :> "cookie" :> Get '[WaargJSON WDJson] (Vector Json))
  :<|> ("session" :> Capture "sessionId" SessionId :> "cookie" :> ReqBody '[WaargJSON WDJson] AddCookie :> Post '[] NoContent)

  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> "active" :> Post '[WaargJSON WDJson] (Value ElementId))
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "attribute" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Value Text))
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "clear" :> Post '[] NoContent)
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "click" :> Post '[] NoContent)
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "css" :> Capture "propertyName" Text :> Get '[WaargJSON WDJson] (Value Text))
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "displayed" :> Get '[WaargJSON WDJson] Bool)
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value ElementId))
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value (Vector ElementId)))
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "enabled" :> Get '[WaargJSON WDJson] Bool)
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "name" :> Get '[WaargJSON WDJson] Text)
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "property" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Value Text))
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "rect" :> Get '[WaargJSON WDJson] Json)
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "screenshot" :> ReqBody '[WaargJSON WDJson] TakeElementScreenshot :> Get '[WaargJSON WDJson] Text)
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "selected" :> Get '[WaargJSON WDJson] Bool)
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "text" :> Get '[WaargJSON WDJson] Text)
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> Capture "elementId" ElementId :> "value" :> ReqBody '[WaargJSON WDJson] ElementSendKeys :> Post '[] NoContent)
  :<|> ("session" :> Capture "sessionId" SessionId :> "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value ElementId))

  :<|> ("session" :> Capture "sessionId" SessionId :> "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Value (Vector ElementId)))

  :<|> ("session" :> Capture "sessionId" SessionId :> "execute" :> "async" :> ReqBody '[WaargJSON WDJson] ExecuteAsyncScript :> Post '[WaargJSON WDJson] Json)
  :<|> ("session" :> Capture "sessionId" SessionId :> "execute" :> "sync" :> ReqBody '[WaargJSON WDJson] ExecuteScript :> Post '[WaargJSON WDJson] Json)

  :<|> ("session" :> Capture "sessionId" SessionId :> "forward" :> Post '[] NoContent)

  :<|> ("session" :> Capture "sessionId" SessionId :> "frame" :> "parent" :> Post '[] NoContent)
  :<|> ("session" :> Capture "sessionId" SessionId :> "frame" :> ReqBody '[WaargJSON WDJson] SwitchToFrame :> Post '[] NoContent)

  :<|> ("session" :> Capture "sessionId" SessionId :> "refresh" :> Post '[] NoContent)

  :<|> ("session" :> Capture "sessionId" SessionId :> "screenshot" :> Get '[WaargJSON WDJson] Text)

  :<|> ("session" :> Capture "sessionId" SessionId :> "source" :> Get '[] NoContent)

  :<|> ("session" :> Capture "sessionId" SessionId :> "timeouts" :> Get '[WaargJSON WDJson] Json)
  :<|> ("session" :> Capture "sessionId" SessionId :> "timeouts" :> ReqBody '[WaargJSON WDJson] Timeout :> Post '[] NoContent)

  :<|> ("session" :> Capture "sessionId" SessionId :> "title" :> Get '[WaargJSON WDJson] Text)

  :<|> ("session" :> Capture "sessionId" SessionId :> "url" :> Get '[WaargJSON WDJson] (Value WDUri))
  :<|> ("session" :> Capture "sessionId" SessionId :> "url" :> ReqBody '[WaargJSON WDJson] WDUri :> Post '[WaargJSON WDJson] (Value ()))

  :<|> ("session" :> Capture "sessionId" SessionId :> "window" :> "fullscreen" :> Post '[WaargJSON WDJson] Json)
  :<|> ("session" :> Capture "sessionId" SessionId :> "window" :> "handles" :> Get '[WaargJSON WDJson] (Vector Text))
  :<|> ("session" :> Capture "sessionId" SessionId :> "window" :> "maximize" :> Post '[WaargJSON WDJson] Json)
  :<|> ("session" :> Capture "sessionId" SessionId :> "window" :> "minimize" :> Post '[WaargJSON WDJson] Json)
  :<|> ("session" :> Capture "sessionId" SessionId :> "window" :> "new" :> ReqBody '[WaargJSON WDJson] CreateWindow :> Post '[WaargJSON WDJson] (Value NewWindow))
  :<|> ("session" :> Capture "sessionId" SessionId :> "window" :> "rect" :> Get '[WaargJSON WDJson] Json)
  :<|> ("session" :> Capture "sessionId" SessionId :> "window" :> "rect" :> ReqBody '[WaargJSON WDJson] SetWindowRect :> Post '[WaargJSON WDJson] Json)
  :<|> ("session" :> Capture "sessionId" SessionId :> "window" :> Delete '[] NoContent)
  :<|> ("session" :> Capture "sessionId" SessionId :> "window" :> Get '[WaargJSON WDJson] (Value WindowHandle))
  :<|> ("session" :> Capture "sessionId" SessionId :> "window" :> ReqBody '[WaargJSON WDJson] SwitchToWindow :> Post '[] NoContent)

  :<|> ("session" :> Capture "sessionId" SessionId :> Delete '[] NoContent)

  :<|> ("session" :> ReqBody '[WaargJSON WDJson] NewSession :> Post '[WaargJSON WDJson] (Value Session))

  :<|> ("status" :> Get '[WaargJSON WDJson] Json)

webdriverApi :: Proxy WebDriverAPI
webdriverApi = Proxy

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
   :<|> deleteSession
   :<|> newSession  :<|> status  = client webdriverApi
