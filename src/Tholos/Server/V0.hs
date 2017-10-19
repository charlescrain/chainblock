{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Tholos.Server.V0
  ( API
  , api
  , server
  ) where

import           Servant

import           Tholos.API.ContentTypes
import           Tholos.API.V0
import           Tholos.App.Transformer  (AppT)
import           Tholos.API.Types
import           Tholos.Types


server :: ServerT API (AppT m)
server = userEntryPoints

userEntryPoints :: ServerT UserSubRouteAPI (AppT m)
userEntryPoints  = getUsersEntryPoint
              :<|> postUserEntryPoint
              :<|> websiteEntryPoints

getUsersEntryPoint :: AppT m [User]
getUsersEntryPoint = throwError $ err500 {errBody="getUsersEntryPoint ** Under Constructions..Nothing to see here"}

postUserEntryPoint :: PostUserBody -> AppT m UserId
postUserEntryPoint _ = throwError $ err500 {errBody="postUserEntryPoint ** Under Constructions..Nothing to see here"}

websiteEntryPoints :: UserId -> ServerT WebsiteSubRouteAPI (AppT m)
websiteEntryPoints uId = getWebsitesEntryPoint uId
                    :<|> postWebsiteEntryPoint uId
                    :<|> postCredentialsEntrypoint uId
                    :<|> getWebsiteCredentialsEntryPoint uId

getWebsitesEntryPoint :: UserId -> AppT m [WebsiteDetails]
getWebsitesEntryPoint _ = throwError err500 {errBody="getWebsitesEntryPoint ** Under Constructions..Nothing to see here"}

postWebsiteEntryPoint :: UserId -> PostWebsite -> AppT m WebsiteId
postWebsiteEntryPoint _ _ = throwError err500 {errBody="postWebsiteEntryPoint ** Under Constructions..Nothing to see here"}

postCredentialsEntrypoint :: UserId -> WebsiteId -> PostCredentials -> AppT m ()
postCredentialsEntrypoint _ _ _ = throwError err500 {errBody="postCredentialsEntrypoint ** Under Constructions..Nothing to see here"}

getWebsiteCredentialsEntryPoint :: UserId -> WebsiteId -> PostMasterKey -> AppT m Website
getWebsiteCredentialsEntryPoint _ _ _ = throwError err500 {errBody="getWebsiteCredentialsEntryPoint ** Under Constructions..Nothing to see here"}
