{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API.V0 where

import           Servant

import           App                         (AppT)
import           ChainBlock.API.ContentTypes
import           ChainBlock.API.Types
import           ChainBlock.Interfaces

type API = "v0" :> UserSubRouteAPI

type UserSubRouteAPI =
        "users" :> Get '[JSONAPI] [User]
   :<|> "users" :> ReqBody '[JSONAPI] PostUserBody :> Post '[JSONAPI] UserId
   :<|> "users" :> Capture "id" UserId :> WebsiteSubRouteAPI

type WebsiteSubRouteAPI =
        "websites" :> Get '[JSONAPI] [WebsiteDetails]
   :<|> "websites" :> ReqBody '[JSONAPI] PostMasterKey :> Post '[JSONAPI] WebsiteId
   :<|> "websites" :> "get" :> Capture "id" WebsiteId :> ReqBody '[JSONAPI] PostMasterKey :> Post '[JSONAPI] Website

api :: Proxy API
api = Proxy

usersApi :: Proxy UserSubRouteAPI
usersApi = Proxy

websiteApi :: Proxy WebsiteSubRouteAPI
websiteApi = Proxy

