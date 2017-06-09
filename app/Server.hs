{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server
  ( server
  ) where

import           Data.Text       (Text)
import           Servant

import           API             (API, SubRoutesAPI)
import           App.Transformer (AppT)
import qualified Server.V0       as V0

server :: ServerT API (AppT m)
server _ = subRoutesServer

subRoutesServer :: ServerT SubRoutesAPI (AppT m)
subRoutesServer = V0.server























-- server :: ServerT API (AppT m)
-- \case
--   Just ("application/vnd.api+json" :: Text) -> subRoutesServer
--   _ -> throwError $ err415 {errBody="Please use 'application/vnd.api+json' as your content-type"}
