# Cautious Sniffle

[W3C Webdriver Protocol](https://w3c.github.io/webdriver/) bindings for Haskell.

Uses [`servant-client`](https://hackage.haskell.org/package/servant-client) to generate
all the functions for us. This library defines the API, types, and their related
structures types, along with their respective JSON encoding/decoding requirements.

This package aims to provide a faithful implementation of the W3C Webdriver protocol and
serve as the basis for more interesting testing and other webdriver related tooling.

The [`clay`](https://hackage.haskell.org/package/clay) package is used for the CSS
location strategies.

### What is that `Success` type?

The `Success` type comes from the goal of being a faithful implementation of the Webdriver
specification. Error and success from Webdriver are described as follows:

> "Errors are represented in the WebDriver protocol by an HTTP response with an HTTP status in the 4xx or 5xx range"

Or

> " A success value has an associated data field which encapsulates the value returned"

A response that is not in the 2xx range is treated as a request failure and results in an
error from Servant. 

A successful request will still return the success "object", which is a trivial wrapper
over the actual result. This package keeps to this pattern and uses the equally trivial `newtype Success a`.

## Capabilities

This package uses the `dependent-map` package to handle the different capability objects
as this gives us the extensibility we need without sacrificing type safety. A dependent
map, or `DMap` is a map where the key determines the type of the value.

```haskell
firefox :: WD.Capabilities
firefox = WD.firefox
  & WD.PlatformName ~=> WD.Linux
  & WD.FirefoxSettings ~=> ffSettings
  where
    ffSettings = mempty
      & FF.FFPrefs ~=> FF.newPrefs "app.update.auto" (FF.TextPref "no")
      & FF.FFArgs ~=> ["--headless"]
```


## Records of functions

The use of the `Generic` functionality within `Servant` has a nice side-effect (heh) that
the API is accessed using records of functions. So a given session is a value, that is a
record containing functions that will only apply to that session. If you select an element
using the `findElement` function from that session, this returns a value which is the
record of functions that will only apply to that element in that session.

This is useful as it frees you from having to keep track of the various identifiers and
helps to avoid having confusing test failures because you've used the wrong identifier
halfway through a larger test.

An example:

```haskell
webdriverExample :: C.ClientM ()
webdriverExample = do
  let
    buttonId = "updatingButton"
    textInputId = "newButtonName"
    textInputValue = "Fred"

    core = WD.mkWDCoreClientM

  -- Create the session within the driver
  newSess <- WD.getSuccessValue <$> WD.newSession
    (WD._core core)
    -- we're going to use FireFox so drop in the FireFox capabilities
    (WD.NewSession firefox Nothing Nothing)

  -- We've created a session, so use the session id to access our client.
  -- NB: This is the only time we'll need to explicity handle an ID
  let sessClient = WD._mkSession core (WD._sessionId newSess)

  -- We use the 'modern-uri' package to handle the creation of correct URLs
  -- This can also be created inline using quasiquotes: [uri|http://foo.com.au|]
  url <- URI.mkURI "http://uitestingplayground.com/textinput"

  -- Tell the driver to load the page
  _ <- WD.navigateTo sessClient (WD.WDUri url)

  -- Some helper functions
  let
    getElem elemTag eid =
      -- Given a CSS identifier, locate an element on the page.
      WD.findElement sessClient (WD.ByCss $ elemTag # byId eid)
      -- Using the found element ID, create a element client for the current session.
      <&> (WD._mkElement core sessClient . WD.getSuccessValue)

    propEq ele prop orig msg = do
      -- Try to retrieve the specified property from the given element.
      p <- WD.getElementProperty ele prop
      -- Check if the value of that property matches our expectations.
      unless (WD.getSuccessValue p == orig) $ error msg

  -- Create element clients for some elements on the web page.
  button <- getElem Clay.button buttonId
  textInput <- getElem Clay.input textInputId

    -- Send some input to the text field
  _ <- WD.elementSendKeys textInput $ WD.ElementSendKeys textInputValue
    -- Send a click to the button element
  _ <- WD.elementClick button

    -- As per the playground exercise, check that the button has the input text as the new value.
  _ <- propEq button "innerHTML" textInputValue "button value mismatch"

    -- Close our session
  void $ WD.deleteSession sessClient
```
