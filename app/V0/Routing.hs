{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module V0.Routing
  ( API
  , api
  , server
  ) where

import           Servant

import           App                         (AppT)
import           ChainBlock.API.ContentTypes
import           ChainBlock.API.Types


type API = "v0" :> UserSubRouteAPI


type UserSubRouteAPI =  "users" :> Get '[JSONAPI] [User]
                   -- "users" :> ReqBody '[JSONAPI] PostUserBody :> Post '[JSONAPI] UserId
--                   :<|> "users" :> Capture "id" UserId :> WebsiteSubRouteAPI
--
-- type WebsiteSubRouteAPI = "websites" :> Get '[JSONAPI] [WebsiteDetails]
--                      :<|> "websites" :> ReqBody '[JSONAPI] PostMasterKey :> Post '[JSONAPI] WebsiteId
--                      :<|> "websites" :> "get" :> Capture "id" WebsiteId :> ReqBody '[JSONAPI] PostMasterKey :> Post '[JSONAPI] Website


api :: Proxy API
api = Proxy

usersApi :: Proxy UserSubRouteAPI
usersApi = Proxy

-- websiteApi :: Proxy WebsiteSubRouteAPI
-- websiteApi = Proxy

server :: ServerT API AppT
server = userEntryPoints

userEntryPoints :: ServerT UserSubRouteAPI AppT
userEntryPoints  =  getUsersEntryPoint
    -- postUserEntryPoint
--              :<|> websiteEntryPoints

getUsersEntryPoint :: AppT [User]
getUsersEntryPoint = throwError $ err500 {errBody="getUsersEntryPoint ** Under Constructions..Nothing to see here"}

postUserEntryPoint :: PostUserBody -> AppT UserId
-- postUserEntryPoint _ = throwError $ err500 {errBody="postUserEntryPoint ** Under Constructions..Nothing to see here"}
postUserEntryPoint  = undefined
--
-- websiteEntryPoints :: UserId -> ServerT WebsiteSubRouteAPI AppT
-- websiteEntryPoints uId = getWebsitesEntryPoint uId
--                 :<|> postWebsiteEntryPoint uId
--                 :<|> getWebsiteCredentialsEntryPoint uId
--
-- getWebsitesEntryPoint :: UserId -> AppT [WebsiteDetails]
-- getWebsitesEntryPoint _ = throwError err500 {errBody="getWebsitesEntryPoint ** Under Constructions..Nothing to see here"}
--
-- postWebsiteEntryPoint :: UserId -> PostMasterKey -> AppT WebsiteId
-- postWebsiteEntryPoint _ _ = throwError err500 {errBody="postWebsiteEntryPoint ** Under Constructions..Nothing to see here"}
--
-- getWebsiteCredentialsEntryPoint :: UserId -> WebsiteId -> PostMasterKey -> AppT Website
-- getWebsiteCredentialsEntryPoint _ _ _ = throwError err500 {errBody="getWebsiteCredentialsEntryPoint ** Under Constructions..Nothing to see here"}
