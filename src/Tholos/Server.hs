{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Tholos.Server
  ( server
  ) where

import           Data.Text       (Text)
import           Servant

import           Tholos.API             (API, SubRoutesAPI)
import           Tholos.App.Transformer (TholosT)
import qualified Tholos.Server.V0       as V0

server :: ServerT API TholosT 
server = subRoutesServer

subRoutesServer :: ServerT SubRoutesAPI TholosT 
subRoutesServer = V0.server























-- server :: ServerT API (TholosT m)
-- \case
--   Just ("application/vnd.api+json" :: Text) -> subRoutesServer
--   _ -> throwError $ err415 {errBody="Please use 'application/vnd.api+json' as your content-type"}
