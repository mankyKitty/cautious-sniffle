{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Protocol.Webdriver.ClientAPI
       (newSession, deleteSession, getTimeouts,
        setTimeouts, getUrl, navigateTo, back, forward,
        refresh, getTitle, getWindowHandle, closeWindow,
        switchToWindow, createWindow, getWindowHandles,
        getWindowRect, setWindowRect, maximizeWindow,
        minimizeWindow, fullscreenWindow, switchToFrame,
        switchToParentFrame, findElement,
        findElementFromElement, findElementsFromElement,
        isElementSelected, isElementDisplayed,
        getElementAttribute, getElementProperty,
        getElementCSSValue, getElementText,
        getElementTagName, getElementRect,
        isElementEnabled, elementClick, elementClear,
        elementSendKeys, takeElementScreenshot,
        getActiveElement, findElements, getPageSource,
        executeScript, executeAsyncScript, getAllCookies,
        addCookie, deleteAllCookies, getNamedCookie,
        deleteCookie, performActions, releaseActions,
        dismissAlert, acceptAlert, getAlertText,
        sendAlertText, takeScreenshot, status)
       where
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Vector (Vector)
import Waargonaut.Types.Json (Json)
import Servant.API
import Servant.Client
import Servant.API.ContentTypes.Waargonaut
       (WaargJSON)
import Protocol.Webdriver.ClientAPI.Types

type WebDriverAPI =
     "session" :>
      ReqBody '[WaargJSON WDJson] NewSession :>
       Post '[WaargJSON WDJson] Json
      :<|>
      "session" :>
       Capture "sessionId" Text :> Delete '[] NoContent
       :<|>
       "status" :> Get '[WaargJSON WDJson] Json :<|>
        "session" :>
         Capture "sessionId" Text :>
          "timeouts" :> Get '[WaargJSON WDJson] Json
         :<|>
         "session" :>
          Capture "sessionId" Text :>
           "timeouts" :>
            ReqBody '[WaargJSON WDJson] SetTimeouts :>
             Post '[] NoContent
          :<|>
          "session" :>
           Capture "sessionId" Text :>
            "url" :> Get '[] NoContent
           :<|>
           "session" :>
            Capture "sessionId" Text :>
             "url" :>
              ReqBody '[WaargJSON WDJson] NavigateTo :>
               Post '[WaargJSON WDJson] Text
            :<|>
            "session" :>
             Capture "sessionId" Text :>
              "back" :> Post '[] NoContent
             :<|>
             "session" :>
              Capture "sessionId" Text :>
               "forward" :> Post '[] NoContent
              :<|>
              "session" :>
               Capture "sessionId" Text :>
                "refresh" :> Post '[] NoContent
               :<|>
               "session" :>
                Capture "sessionId" Text :>
                 "title" :> Get '[WaargJSON WDJson] Text
                :<|>
                "session" :>
                 Capture "sessionId" Text :>
                  "window" :> Get '[WaargJSON WDJson] Text
                 :<|>
                 "session" :>
                  Capture "sessionId" Text :>
                   "window" :> Delete '[] NoContent
                  :<|>
                  "session" :>
                   Capture "sessionId" Text :>
                    "window" :>
                     ReqBody '[WaargJSON WDJson] SwitchToWindow :>
                      Post '[] NoContent
                   :<|>
                   "session" :>
                    Capture "sessionId" Text :>
                     "window" :>
                      "new" :>
                       ReqBody '[WaargJSON WDJson] CreateWindow :>
                        Post '[WaargJSON WDJson] Json
                    :<|>
                    "session" :>
                     Capture "sessionId" Text :>
                      "window" :>
                       "handles" :> Get '[WaargJSON WDJson] (Vector Text)
                     :<|>
                     "session" :>
                      Capture "sessionId" Text :>
                       "frame" :>
                        ReqBody '[WaargJSON WDJson] SwitchToFrame :>
                         Post '[] NoContent
                      :<|>
                      "session" :>
                       Capture "sessionId" Text :>
                        "frame" :> "parent" :> Post '[] NoContent
                       :<|>
                       "session" :>
                        Capture "sessionId" Text :>
                         "window" :> "rect" :> Get '[WaargJSON WDJson] Json
                        :<|>
                        "session" :>
                         Capture "sessionId" Text :>
                          "window" :>
                           "rect" :>
                            ReqBody '[WaargJSON WDJson] SetWindowRect :>
                             Post '[WaargJSON WDJson] Json
                         :<|>
                         "session" :>
                          Capture "sessionId" Text :>
                           "window" :>
                            "maximize" :> Post '[WaargJSON WDJson] Json
                          :<|>
                          "session" :>
                           Capture "sessionId" Text :>
                            "window" :>
                             "minimize" :> Post '[WaargJSON WDJson] Json
                           :<|>
                           "session" :>
                            Capture "sessionId" Text :>
                             "window" :>
                              "fullscreen" :> Post '[WaargJSON WDJson] Json
                            :<|>
                            "session" :>
                             Capture "sessionId" Text :>
                              "element" :>
                               ReqBody '[WaargJSON WDJson] FindElement :>
                                Post '[WaargJSON WDJson] Text
                             :<|>
                             "session" :>
                              Capture "sessionId" Text :>
                               "elements" :>
                                ReqBody '[WaargJSON WDJson] FindElements :>
                                 Post '[WaargJSON WDJson] (Vector Text)
                              :<|>
                              "session" :>
                               Capture "sessionId" Text :>
                                "element" :>
                                 Capture "elementId" Text :>
                                  "element" :>
                                   ReqBody '[WaargJSON WDJson] FindElementFromElement
                                    :> Post '[WaargJSON WDJson] Text
                               :<|>
                               "session" :>
                                Capture "sessionId" Text :>
                                 "element" :>
                                  Capture "elementId" Text :>
                                   "elements" :>
                                    ReqBody '[WaargJSON WDJson]
                                     FindElementsFromElement
                                     :> Post '[WaargJSON WDJson] (Vector Text)
                                :<|>
                                "session" :>
                                 Capture "sessionId" Text :>
                                  "element" :>
                                   "active" :> Post '[WaargJSON WDJson] Text
                                 :<|>
                                 "session" :>
                                  Capture "sessionId" Text :>
                                   "element" :>
                                    Capture "elementId" Text :>
                                     "selected" :> Get '[WaargJSON WDJson] Bool
                                  :<|>
                                  "session" :>
                                   Capture "sessionId" Text :>
                                    "element" :>
                                     Capture "elementId" Text :>
                                      "displayed" :> Get '[WaargJSON WDJson] Bool
                                   :<|>
                                   "session" :>
                                    Capture "sessionId" Text :>
                                     "element" :>
                                      Capture "elementId" Text :>
                                       "attribute" :>
                                        Capture "name" Text :>
                                         Get '[WaargJSON WDJson] Text
                                    :<|>
                                    "session" :>
                                     Capture "sessionId" Text :>
                                      "element" :>
                                       Capture "elementId" Text :>
                                        "property" :>
                                         Capture "name" Text :>
                                          Get '[WaargJSON WDJson] Text
                                     :<|>
                                     "session" :>
                                      Capture "sessionId" Text :>
                                       "element" :>
                                        Capture "elementId" Text :>
                                         "css" :>
                                          Capture "propertyName" Text :>
                                           Get '[WaargJSON WDJson] Text
                                      :<|>
                                      "session" :>
                                       Capture "sessionId" Text :>
                                        "element" :>
                                         Capture "elementId" Text :>
                                          "text" :> Get '[WaargJSON WDJson] Text
                                       :<|>
                                       "session" :>
                                        Capture "sessionId" Text :>
                                         "element" :>
                                          Capture "elementId" Text :>
                                           "name" :> Get '[WaargJSON WDJson] Text
                                        :<|>
                                        "session" :>
                                         Capture "sessionId" Text :>
                                          "element" :>
                                           Capture "elementId" Text :>
                                            "rect" :> Get '[WaargJSON WDJson] Json
                                         :<|>
                                         "session" :>
                                          Capture "sessionId" Text :>
                                           "element" :>
                                            Capture "elementId" Text :>
                                             "enabled" :> Get '[WaargJSON WDJson] Bool
                                          :<|>
                                          "session" :>
                                           Capture "sessionId" Text :>
                                            "element" :>
                                             Capture "elementId" Text :>
                                              "click" :> Post '[] NoContent
                                           :<|>
                                           "session" :>
                                            Capture "sessionId" Text :>
                                             "element" :>
                                              Capture "elementId" Text :>
                                               "clear" :> Post '[] NoContent
                                            :<|>
                                            "session" :>
                                             Capture "sessionId" Text :>
                                              "element" :>
                                               Capture "elementId" Text :>
                                                "value" :>
                                                 ReqBody '[WaargJSON WDJson] ElementSendKeys :>
                                                  Post '[] NoContent
                                             :<|>
                                             "session" :>
                                              Capture "sessionId" Text :>
                                               "source" :> Get '[] NoContent
                                              :<|>
                                              "session" :>
                                               Capture "sessionId" Text :>
                                                "execute" :>
                                                 "sync" :>
                                                  ReqBody '[WaargJSON WDJson] ExecuteScript :>
                                                   Post '[WaargJSON WDJson] Json
                                               :<|>
                                               "session" :>
                                                Capture "sessionId" Text :>
                                                 "execute" :>
                                                  "async" :>
                                                   ReqBody '[WaargJSON WDJson] ExecuteAsyncScript :>
                                                    Post '[WaargJSON WDJson] Json
                                                :<|>
                                                "session" :>
                                                 Capture "sessionId" Text :>
                                                  "cookie" :> Get '[WaargJSON WDJson] (Vector Json)
                                                 :<|>
                                                 "session" :>
                                                  Capture "sessionId" Text :>
                                                   "cookie" :>
                                                    ReqBody '[WaargJSON WDJson] AddCookie :>
                                                     Post '[] NoContent
                                                  :<|>
                                                  "session" :>
                                                   Capture "sessionId" Text :>
                                                    "cookie" :> Delete '[] NoContent
                                                   :<|>
                                                   "session" :>
                                                    Capture "sessionId" Text :>
                                                     "cookie" :>
                                                      Capture "name" Text :>
                                                       Get '[WaargJSON WDJson] Json
                                                    :<|>
                                                    "session" :>
                                                     Capture "sessionId" Text :>
                                                      "cookie" :>
                                                       Capture "name" Text :> Delete '[] NoContent
                                                     :<|>
                                                     "session" :>
                                                      Capture "sessionId" Text :>
                                                       "actions" :>
                                                        ReqBody '[WaargJSON WDJson] PerformActions
                                                         :> Post '[] NoContent
                                                      :<|>
                                                      "session" :>
                                                       Capture "sessionId" Text :>
                                                        "actions" :> Delete '[] NoContent
                                                       :<|>
                                                       "session" :>
                                                        Capture "sessionId" Text :>
                                                         "alert" :> "dismiss" :> Post '[] NoContent
                                                        :<|>
                                                        "session" :>
                                                         Capture "sessionId" Text :>
                                                          "alert" :> "accept" :> Post '[] NoContent
                                                         :<|>
                                                         "session" :>
                                                          Capture "sessionId" Text :>
                                                           "alert" :>
                                                            "text" :> Get '[WaargJSON WDJson] Text
                                                          :<|>
                                                          "session" :>
                                                           Capture "sessionId" Text :>
                                                            "alert" :>
                                                             "text" :>
                                                              ReqBody '[WaargJSON WDJson]
                                                               SendAlertText
                                                               :> Post '[] NoContent
                                                           :<|>
                                                           "session" :>
                                                            Capture "sessionId" Text :>
                                                             "screenshot" :>
                                                              Get '[WaargJSON WDJson] Text
                                                            :<|>
                                                            "session" :>
                                                             Capture "sessionId" Text :>
                                                              "element" :>
                                                               Capture "elementId" Text :>
                                                                "screenshot" :>
                                                                 ReqBody '[WaargJSON WDJson]
                                                                  TakeElementScreenshot
                                                                  :> Get '[WaargJSON WDJson] Text

webdriverApi :: Proxy WebDriverAPI
webdriverApi = Proxy
newSession :<|>
 (deleteSession :<|>
   (getTimeouts :<|>
     (setTimeouts :<|>
       (getUrl :<|>
         (navigateTo :<|>
           (back :<|>
             (forward :<|>
               (refresh :<|>
                 (getTitle :<|>
                   (getWindowHandle :<|>
                     (closeWindow :<|>
                       (switchToWindow :<|>
                         (createWindow :<|>
                           (getWindowHandles :<|>
                             (getWindowRect :<|>
                               (setWindowRect :<|>
                                 (maximizeWindow :<|>
                                   (minimizeWindow :<|>
                                     (fullscreenWindow :<|>
                                       (switchToFrame :<|>
                                         (switchToParentFrame :<|>
                                           (findElement :<|>
                                             (findElementFromElement :<|>
                                               (findElementsFromElement :<|>
                                                 (isElementSelected :<|>
                                                   (isElementDisplayed :<|>
                                                     (getElementAttribute :<|>
                                                       (getElementProperty :<|>
                                                         (getElementCSSValue :<|>
                                                           (getElementText :<|>
                                                             (getElementTagName :<|>
                                                               (getElementRect :<|>
                                                                 (isElementEnabled :<|>
                                                                   (elementClick :<|>
                                                                     (elementClear :<|>
                                                                       (elementSendKeys :<|>
                                                                         (takeElementScreenshot :<|>
                                                                           (getActiveElement :<|>
                                                                             (findElements :<|>
                                                                               (getPageSource :<|>
                                                                                 (executeScript :<|>
                                                                                   (executeAsyncScript
                                                                                     :<|>
                                                                                     (getAllCookies
                                                                                       :<|>
                                                                                       (addCookie
                                                                                         :<|>
                                                                                         (deleteAllCookies
                                                                                           :<|>
                                                                                           (getNamedCookie
                                                                                             :<|>
                                                                                             (deleteCookie
                                                                                               :<|>
                                                                                               (performActions
                                                                                                 :<|>
                                                                                                 (releaseActions
                                                                                                   :<|>
                                                                                                   (dismissAlert
                                                                                                     :<|>
                                                                                                     (acceptAlert
                                                                                                       :<|>
                                                                                                       (getAlertText
                                                                                                         :<|>
                                                                                                         (sendAlertText
                                                                                                           :<|>
                                                                                                           (takeScreenshot
                                                                                                             :<|>
                                                                                                             status))))))))))))))))))))))))))))))))))))))))))))))))))))))
 = client webdriverApi
