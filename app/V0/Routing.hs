{-# LANGUAGE DataKinds, TypeOperators #-}

module V0.Routing
  ( API
  , api
  , server
  ) where

import Servant

import App (AppT)


type API = "v0" :> Get '[JSON] [String]

api :: Proxy API
api = Proxy

server :: ServerT API AppT
server = dummy

dummy :: AppT [String]
dummy = return ["hi"]
