{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Servant.API.Client.HollowBody where

import           Data.Kind           (Type)
import           Data.Proxy          (Proxy (..))
import           Data.Typeable       (Typeable)

import           Servant.API         ((:>), HasLink (..), MimeRender (..),
                                      contentType)
import           Servant.Client      (HasClient (..))
import           Servant.Client.Core (setRequestBodyLBS)

data HollowBody (contentTypes :: [*]) deriving Typeable

instance HasLink sub => HasLink (HollowBody contentTypes :> sub :: Type) where
  type MkLink (HollowBody contentTypes :> sub) a = MkLink sub a
  toLink toA _ = toLink toA (Proxy :: Proxy sub)

instance (MimeRender ct (), HasClient m api) => HasClient m (HollowBody (ct ': cts) :> api) where
  type Client m (HollowBody (ct ': cts) :> api) = Client m api
  clientWithRoute pm Proxy req = clientWithRoute pm (Proxy :: Proxy api) req0
    where
      ctProxy = Proxy :: Proxy ct
      req0 = setRequestBodyLBS (mimeRender ctProxy ()) (contentType ctProxy) req

  hoistClientMonad pm _ f cl =
    hoistClientMonad pm (Proxy :: Proxy api) f cl
