{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Routing
  ( API
  , api
  , server
  ) where

import           Data.Text  (Text)
import           Servant

import           App        (AppT)
import qualified V0.Routing as V0


-- type API = "api" :> SubRoutesAPI
type API = "api" :> Header "Accept" Text :> SubRoutesAPI

type SubRoutesAPI = V0.API


api :: Proxy API
api = Proxy

server  = \case
  Just ("application/vnd.api+json" :: Text) -> subRoutesServer
  _ -> throwError $ err415 {errBody="Please use 'application/vnd.api+json' as your content-type"}

subRoutesServer :: ServerT V0.API AppT
subRoutesServer = V0.server
