{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.V0
  ( API
  , api
  , server
  ) where

import           Servant

import           API.ContentTypes
import           API.V0
import           App.Transformer  (AppT)
import           Tholos.API.Types


server :: ServerT API (AppT m m')
server = userEntryPoints

userEntryPoints :: ServerT UserSubRouteAPI (AppT m m')
userEntryPoints  = getUsersEntryPoint
              :<|> postUserEntryPoint
              :<|> websiteEntryPoints

getUsersEntryPoint :: AppT m m' [User]
getUsersEntryPoint = throwError $ err500 {errBody="getUsersEntryPoint ** Under Constructions..Nothing to see here"}

postUserEntryPoint :: PostUserBody -> AppT m m' UserId
postUserEntryPoint _ = throwError $ err500 {errBody="postUserEntryPoint ** Under Constructions..Nothing to see here"}

websiteEntryPoints :: UserId -> ServerT WebsiteSubRouteAPI (AppT m m')
websiteEntryPoints uId = getWebsitesEntryPoint uId
                    :<|> postWebsiteEntryPoint uId
                    :<|> postCredentialsEntrypoint uId
                    :<|> getWebsiteCredentialsEntryPoint uId

getWebsitesEntryPoint :: UserId -> AppT m m' [WebsiteDetails]
getWebsitesEntryPoint _ = throwError err500 {errBody="getWebsitesEntryPoint ** Under Constructions..Nothing to see here"}

postWebsiteEntryPoint :: UserId -> PostWebsite -> AppT m m' WebsiteId
postWebsiteEntryPoint _ _ = throwError err500 {errBody="postWebsiteEntryPoint ** Under Constructions..Nothing to see here"}

postCredentialsEntrypoint :: UserId -> WebsiteId -> PostCredentials -> AppT m m' ()
postCredentialsEntrypoint _ _ _ = throwError err500 {errBody="postCredentialsEntrypoint ** Under Constructions..Nothing to see here"}

getWebsiteCredentialsEntryPoint :: UserId -> WebsiteId -> PostMasterKey -> AppT m m' Website
getWebsiteCredentialsEntryPoint _ _ _ = throwError err500 {errBody="getWebsiteCredentialsEntryPoint ** Under Constructions..Nothing to see here"}
