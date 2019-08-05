{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
-- | This module contains the records that represent the WebDriver commands for Sessions,
-- Elements, and Window controls.
--
module Protocol.Webdriver.ClientAPI.SessionAPI
  ( -- * API records
    SessionAPI (..)
  , ElementAPI (..)
  , WindowAPI (..)

    -- * Proxy values
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

-- | Record containing the commands for interacting with an element.
data ElementAPI route = ElementAPI
  { -- | Try to retrieve the value of the given attribute. [spec](https://w3c.github.io/webdriver/#get-element-attribute) 
    getElementAttribute     :: route :- "attribute" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Success ByteString)
    -- | Clear the contents of an editable element. [spec](https://w3c.github.io/webdriver/#element-clear)
  , elementClear            :: route :- "clear" :> HollowBody '[WaargJSON WDJson] :> Post '[WaargJSON WDJson] (Success ())
    -- | Click the center point of the element. [spec](https://w3c.github.io/webdriver/#element-click)
  , elementClick            :: route :- "click" :> HollowBody '[WaargJSON WDJson] :> Post '[WaargJSON WDJson] (Success ())
    -- | Try to retrieve the value of the given CSS property. [spec](https://w3c.github.io/webdriver/#get-element-css-value)
  , getElementCSSValue      :: route :- "css" :> Capture "propertyName" Text :> Get '[WaargJSON WDJson] (Success PropertyVal)
    -- | Check the "displayedness" of the element. [spec](https://w3c.github.io/webdriver/#element-displayedness)
  , isElementDisplayed      :: route :- "displayed" :> Get '[WaargJSON WDJson] (Success Bool)
    -- | Find an element starting from the current element. [spec](https://w3c.github.io/webdriver/#find-element-from-element)
  , findElementFromElement  :: route :- "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Success ElementId)
    -- | Find a list of elements from the current element. [spec](https://w3c.github.io/webdriver/#find-elements-from-element)
  , findElementsFromElement :: route :- "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Success (Vector ElementId))
    -- | Check if the element is enabled. [spec](https://w3c.github.io/webdriver/#is-element-enabled)
  , isElementEnabled        :: route :- "enabled" :> Get '[WaargJSON WDJson] (Success Bool)
    -- | Retrieve the HTML tag name of the current element. [spec](https://w3c.github.io/webdriver/#get-element-tag-name)
  , getElementTagName       :: route :- "name" :> Get '[WaargJSON WDJson] (Success Text)
    -- | Access the given property from the element object. [spec](https://w3c.github.io/webdriver/#get-element-property)
  , getElementProperty      :: route :- "property" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Success PropertyVal)
    -- | Get the bounding rectangle of the current element, in CSS pixels. [spec](https://w3c.github.io/webdriver/#get-element-rect)
  , getElementRect          :: route :- "rect" :> Get '[WaargJSON WDJson] (Success WDRect)
    -- | Take a screenshot encompassed by the bounding rectangle of the element. [spec](https://w3c.github.io/webdriver/#take-element-screenshot)
  , takeElementScreenshot   :: route :- "screenshot" :> ReqBody '[WaargJSON WDJson] TakeElementScreenshot :> Get '[WaargJSON WDJson] (Success Base64)
    -- | Check if the element is selected. [spec](https://w3c.github.io/webdriver/#is-element-selected)
  , isElementSelected       :: route :- "selected" :> Get '[WaargJSON WDJson] (Success Bool)
    -- | Retrieve the element's text "as rendered". [spec](https://w3c.github.io/webdriver/#get-element-text)
  , getElementText          :: route :- "text" :> Get '[WaargJSON WDJson] (Success Text)
    -- | Send a sequence of key inputs to the element. [spec](https://w3c.github.io/webdriver/#element-send-keys)
  , elementSendKeys         :: route :- "value" :> ReqBody '[WaargJSON WDJson] ElementSendKeys :> Post '[WaargJSON WDJson] (Success ())
  }
  deriving GHC.Generic

-- | Proxy value for 'ElementAPI'
elemApi :: Proxy (ToServantApi ElementAPI)
elemApi = genericApi (Proxy @ElementAPI)

-- | Record containing commands for interacting with the window for the current session.
data WindowAPI route = WindowAPI
  { -- | Set the current window to take up the full screen. Returns the 'WDRect' of the size of the resulting window. [spec](https://w3c.github.io/webdriver/#fullscreen-window)
    fullscreenWindow        :: route :- "fullscreen" :> Post '[WaargJSON WDJson] (Success WDRect)
    -- | Returns a list of the window handles associated with the current session. [spec](https://w3c.github.io/webdriver/#get-window-handles)
  , getWindowHandles        :: route :- "handles" :> Get '[WaargJSON WDJson] (Vector WindowHandle)
    -- | Maximise the current window. [spec](https://w3c.github.io/webdriver/#maximize-window)
  , maximizeWindow          :: route :- "maximize" :> Post '[WaargJSON WDJson] (Success WDRect)
    -- | Minimise the current window. [spec](https://w3c.github.io/webdriver/#minimize-window)
  , minimizeWindow          :: route :- "minimize" :> Post '[WaargJSON WDJson] (Success WDRect)
    -- | Create a new window. [spec](https://w3c.github.io/webdriver/#new-window)
  , createWindow            :: route :- "new" :> ReqBody '[WaargJSON WDJson] CreateWindow :> Post '[WaargJSON WDJson] (Success NewWindow)
    -- | Retrieve the size of the current window rectangle. [spec](https://w3c.github.io/webdriver/#get-window-rect)
  , getWindowRect           :: route :- "rect" :> Get '[WaargJSON WDJson] (Success WDRect)
    -- | Resize and reposition the current window. [spec](https://w3c.github.io/webdriver/#set-window-rect)
  , setWindowRect           :: route :- "rect" :> ReqBody '[WaargJSON WDJson] WDRect :> Post '[WaargJSON WDJson] (Success WDRect)
    -- | Close the current window. [spec](https://w3c.github.io/webdriver/#close-window)
  , closeWindow             :: route :- Delete '[WaargJSON WDJson] (Success (Vector WindowHandle))
    -- | Retrieve the current window handle. [spec](https://w3c.github.io/webdriver/#get-window-handle)
  , getWindowHandle         :: route :- Get '[WaargJSON WDJson] (Success WindowHandle)
    -- | Switch to the given window. [spec](https://w3c.github.io/webdriver/#switch-to-window)
  , switchToWindow          :: route :- ReqBody '[WaargJSON WDJson] SwitchToWindow :> Post '[WaargJSON WDJson] (Success ())
  }
  deriving GHC.Generic

-- | Proxy value for 'WindowAPI'
windowApi :: Proxy (ToServantApi WindowAPI)
windowApi = genericApi (Proxy @WindowAPI)

-- | Record containing commands for interacting with the current session. Also contains
-- the commands for accessing specific 'ElementAPI' and 'WindowAPI' records.
data SessionAPI route = SessionAPI
  { -- | Release all keys and mouse buttons that are currently depressed. [spec](https://w3c.github.io/webdriver/#release-actions)
    releaseActions          :: route :- "actions" :> Delete '[WaargJSON WDJson] (Success ())
    -- | Send sequence of pointer or key instructions to the browser. [spec](https://w3c.github.io/webdriver/#perform-actions)
  , performActions          :: route :- "actions" :> ReqBody '[WaargJSON WDJson] PerformActions :> Post '[WaargJSON WDJson] (Success ())
    -- | Accept the current prompt, errors if there is no prompt. [spec](https://w3c.github.io/webdriver/#accept-alert)
  , acceptAlert             :: route :- "alert" :> "accept" :> HollowBody '[WaargJSON WDJson] :> Post '[WaargJSON WDJson] (Success ())
    -- | Dismiss the current prompt, may be indistinguishable from accepting the prompt. [spec](https://w3c.github.io/webdriver/#dismiss-alert)
  , dismissAlert            :: route :- "alert" :> "dismiss" :> HollowBody '[WaargJSON WDJson] :> Post '[WaargJSON WDJson] (Success ())
    -- | Retrieve the text of the current prompt. [spec](https://w3c.github.io/webdriver/#get-alert-text)
  , getAlertText            :: route :- "alert" :> "text" :> Get '[WaargJSON WDJson] (Success Text)
    -- | Set the text of the current @window.prompt@. [spec](https://w3c.github.io/webdriver/#send-alert-text)
  , sendAlertText           :: route :- "alert" :> "text" :> ReqBody '[WaargJSON WDJson] SendAlertText :> Post '[WaargJSON WDJson] (Success ())
    -- | Trigger browser back, equivalent to @window.history.back@. [spec](https://w3c.github.io/webdriver/#back)
  , back                    :: route :- "back" :> Post '[WaargJSON WDJson] (Success ())
    -- | Delete the cookie with the given name. [spec](https://w3c.github.io/webdriver/#delete-cookie)
  , deleteCookie            :: route :- "cookie" :> Capture "name" Text :> Delete '[WaargJSON WDJson] (Success ())
    -- | Retrieve the cookie that has the given name. [spec](https://w3c.github.io/webdriver/#get-named-cookie)
  , getNamedCookie          :: route :- "cookie" :> Capture "name" Text :> Get '[WaargJSON WDJson] (Success Cookie)
    -- | Delete all the cookies in the current session. [spec](https://w3c.github.io/webdriver/#delete-all-cookies)
  , deleteAllCookies        :: route :- "cookie" :> Delete '[WaargJSON WDJson] (Success ())
    -- | Retrieve all the cookies from the current session. [spec](https://w3c.github.io/webdriver/#get-all-cookies)
  , getAllCookies           :: route :- "cookie" :> Get '[WaargJSON WDJson] (Success (Vector Cookie))
    -- | Add the given cookie to the current session. [spec](https://w3c.github.io/webdriver/#add-cookie)
  , addCookie               :: route :- "cookie" :> ReqBody '[WaargJSON WDJson] Cookie :> Post '[WaargJSON WDJson] (Success ())
    -- | Retrieve the currently active element. [spec](https://w3c.github.io/webdriver/#get-active-element)
  , getActiveElement        :: route :- "element" :> "active" :> Get '[WaargJSON WDJson] (Success ElementId)
    -- | Return an 'ElementAPI' for the given 'ElementId' in the current session.
  , withElement             :: route :- "element" :> Capture "elementId" ElementId :> ToServantApi ElementAPI
    -- | Locate the element on the page in the current session. [spec](https://w3c.github.io/webdriver/#find-element)
  , findElement             :: route :- "element" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Success ElementId)
    -- | Locate all the elements that match the given location strategy. [spec](https://w3c.github.io/webdriver/#find-elements)
  , findElements            :: route :- "elements" :> ReqBody '[WaargJSON WDJson] LocateUsing :> Post '[WaargJSON WDJson] (Success (Vector ElementId))
    -- | Execute asychronous JavaScript code in the current session. [spec](https://w3c.github.io/webdriver/#execute-async-script)
  , executeAsyncScript      :: route :- "execute" :> "async" :> ReqBody '[WaargJSON WDJson] ExecuteAsyncScript :> Post '[WaargJSON WDJson] (Success Json)
    -- | Execute synchronous JavaScript code in the current session. [spec](https://w3c.github.io/webdriver/#execute-script)
  , executeScript           :: route :- "execute" :> "sync" :> ReqBody '[WaargJSON WDJson] ExecuteScript :> Post '[WaargJSON WDJson] (Success Json)
    -- | Go 'forward' in the history of the current session. [spec](https://w3c.github.io/webdriver/#forward)
  , forward                 :: route :- "forward" :> Post '[WaargJSON WDJson] (Success ())
    -- | Switch to the parent of the current frame. [spec](https://w3c.github.io/webdriver/#switch-to-parent-frame)
  , switchToParentFrame     :: route :- "frame" :> "parent" :> Post '[WaargJSON WDJson] (Success ())
    -- | Switch to the given frame using the handle in 'SwitchToFrame'. [spec](https://w3c.github.io/webdriver/#switch-to-frame)
  , switchToFrame           :: route :- "frame" :> ReqBody '[WaargJSON WDJson] SwitchToFrame :> Post '[WaargJSON WDJson] (Success ())
    -- | Refresh the current page. [spec](https://w3c.github.io/webdriver/#refresh)
  , refresh                 :: route :- "refresh" :> HollowBody '[WaargJSON WDJson] :> Post '[WaargJSON WDJson] (Success ())
    -- | Take a screenshot of the entire document element rectangle. [spec](https://w3c.github.io/webdriver/#take-screenshot)
  , takeScreenshot          :: route :- "screenshot" :> Get '[WaargJSON WDJson] (Success Base64)
    -- | Retrieve the source of the current page. [spec](https://w3c.github.io/webdriver/#get-page-source)
  , getPageSource           :: route :- "source" :> Get '[WaargJSON WDJson] (Success Text)
    -- | Retrieve the active sessions timeouts. [spec](https://w3c.github.io/webdriver/#get-timeouts)
  , getTimeouts             :: route :- "timeouts" :> Get '[WaargJSON WDJson] (Success Timeout)
    -- | Set the timeout in the current session. [spec](https://w3c.github.io/webdriver/#set-timeouts)
  , setTimeouts             :: route :- "timeouts" :> ReqBody '[WaargJSON WDJson] Timeout :> Post '[WaargJSON WDJson] (Success ())
    -- | Retrieve the title of the current page. Equivalent to @document.title@. [spec](https://w3c.github.io/webdriver/#get-title)
  , getTitle                :: route :- "title" :> Get '[WaargJSON WDJson] (Success Text)
    -- | Retrieve the URL of the current page. [spec](https://w3c.github.io/webdriver/#get-current-url)
  , getCurrentUrl                  :: route :- "url" :> Get '[WaargJSON WDJson] (Success WDUri)
    -- | Navigate the current session to the given URL. [spec](https://w3c.github.io/webdriver/#navigate-to)
  , navigateTo              :: route :- "url" :> ReqBody '[WaargJSON WDJson] WDUri :> Post '[WaargJSON WDJson] (Success ())
    -- | Return the 'WindowAPI' for the current session.
  , withWindow              :: route :- "window" :> ToServantApi WindowAPI
    -- | Delete this session. [spec](https://w3c.github.io/webdriver/#delete-session)
  , deleteSession           :: route :- Delete '[WaargJSON WDJson] (Success ())
  }
  deriving GHC.Generic

-- | Proxy value for 'SessionAPI'
sessionApi :: Proxy (ToServantApi SessionAPI)
sessionApi = genericApi (Proxy @SessionAPI)
