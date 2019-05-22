# Cautious Sniffle

[W3C Webdriver Protocol](https://w3c.github.io/webdriver/) bindings for Haskell.
Using `servant-client` to generate all the functions for us. This library
defines the API types and their related protocol types, along with their
respective JSON encoding/decoding requirements.

Looking for help to get this pushed over the line!! :) 

## TODO

- [ ] Flesh out all the remaining types to more concrete definitions than `Json`.
- [ ] Create & maintain documentation for api and types
- [ ] Add as many tests as possible.
- [ ] Maintain the example so people can see this package being used.

Update/Delete the code-gen, it's currently out of date but I haven't made a call
to delete it or upgrade it. The API might be easier to maintain as it is,
courtesy of the awesomeness of servant-client & Generics.