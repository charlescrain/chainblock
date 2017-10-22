{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Tholos.Server.V0 where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT)
import           Servant

import           Tholos.API.ContentTypes
import           Tholos.API.Class
import           Tholos.API.V0
import           Tholos.App.Transformer  (TholosT)
import           Tholos.API.Types
import           Tholos.AppConfig          (AppConfig)
import           Tholos.Types
import           Tholos.Errors


server :: ServerT API (TholosT)
server = userEntryPoints

userEntryPoints :: ServerT UserSubRouteAPI (TholosT)
userEntryPoints  = getUsersEntryPoint
              :<|> postUserEntryPoint
              :<|> websiteEntryPoints

getUsersEntryPoint :: ( MonadIO m
                      , DBQueryUser m
                      ) => m [User]
getUsersEntryPoint = getUsers

postUserEntryPoint :: ( MonadIO m
                      , DBModifyUser m
                      ) => Username -> m UserId
postUserEntryPoint un = insertUser un

websiteEntryPoints :: UserId -> ServerT WebsiteSubRouteAPI (TholosT)
websiteEntryPoints uId = getWebsitesEntryPoint uId
                    :<|> postWebsiteEntryPoint uId
                    :<|> postCredentialsEntrypoint uId
                    :<|> getWebsiteCredentialsEntryPoint uId

getWebsitesEntryPoint :: UserId -> TholosT [WebsiteDetails]
getWebsitesEntryPoint _ = undefined

postWebsiteEntryPoint :: ( MonadIO m
                         , DBModifyWebsite m
                         ) => UserId -> PostWebsite -> m WebsiteId
postWebsiteEntryPoint _ _ = undefined

postCredentialsEntrypoint :: UserId -> WebsiteId -> PostCredentials -> TholosT ()
postCredentialsEntrypoint _ _ _ = undefined

getWebsiteCredentialsEntryPoint :: UserId -> WebsiteId -> PostMasterKey -> TholosT Website
getWebsiteCredentialsEntryPoint _ _ _ = undefined
