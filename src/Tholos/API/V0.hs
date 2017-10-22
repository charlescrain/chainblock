{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Tholos.API.V0 where

import           Servant

import           Tholos.API.ContentTypes
import           Tholos.App.Transformer  (TholosT)
import           Tholos.API.Types
import           Tholos.Types

type API = "v0" :> UserSubRouteAPI

type UserSubRouteAPI =
        "users" :> Get '[JSONAPI] [User]
   :<|> "users" :> ReqBody '[JSONAPI] Username :> Post '[JSONAPI] UserId
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

