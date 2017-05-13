{-# LANGUAGE DataKinds, TypeOperators #-}

module Routing
  ( API
  , api
  , server
  ) where

import Servant

import App (AppT)
import qualified V0.Routing as V0


type API = "api" :> SubRoutesAPI

type SubRoutesAPI = V0.API

api :: Proxy API
api = Proxy

server :: ServerT API AppT
server = subRoutesServer

subRoutesServer :: ServerT V0.API AppT
subRoutesServer = V0.server
