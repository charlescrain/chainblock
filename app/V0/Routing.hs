{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module V0.Routing
  ( API
  , api
  , server
  ) where

import           Servant

import           App                         (AppT)
import           ChainBlock.API.ContentTypes


type API = "v0" :> Get '[JSONAPI] [String]

-- type UserSubRouteAPI =
--   "users" :> Get '[JSON] [User]


api :: Proxy API
api = Proxy

server :: ServerT API AppT
server = dummy

dummy :: AppT [String]
dummy = return ["hi"]
