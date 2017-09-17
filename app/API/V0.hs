{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API.V0 where

import           Servant

import           API.ContentTypes
import           App.Transformer      (AppT)
import           ChainBlock.API.Types

type API = "v0" :> UserSubRouteAPI

type UserSubRouteAPI =
        "users" :> Get '[JSONAPI] [User]
   :<|> "users" :> ReqBody '[JSONAPI] PostUserBody :> Post '[JSONAPI] UserId
   :<|> "users" :> Capture "id" UserId :> WebsiteSubRouteAPI

type WebsiteSubRouteAPI =
        "websites" :> Get '[JSONAPI] [WebsiteDetails]
   :<|> "websites"
      :> ReqBody '[JSONAPI] PostWebsite
      :> Post '[JSONAPI] WebsiteId
   :<|> "websites"
      :> Capture "id" WebsiteId
      :> "credentials"
      :> ReqBody '[JSONAPI] PostCredentials
      :> Post '[JSONAPI] ()
   :<|> "websites"
      :> "get"
      :> Capture "id" WebsiteId
      :> ReqBody '[JSONAPI] PostMasterKey
      :> Post '[JSONAPI] Website

api :: Proxy API
api = Proxy

usersApi :: Proxy UserSubRouteAPI
usersApi = Proxy

websiteApi :: Proxy WebsiteSubRouteAPI
websiteApi = Proxy

