{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Tholos.Server.V0 where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT)
import           Servant

import           Tholos.API.ContentTypes
import           Tholos.API.Class
import           Tholos.API.V0
import           Tholos.App.Transformer  (AppT)
import           Tholos.API.Types
import           Tholos.App.Config          (AppConfig)
import           Tholos.Types
import           Tholos.Errors


server :: ServerT API (AppT)
server = userEntryPoints

userEntryPoints :: ServerT UserSubRouteAPI (AppT)
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

websiteEntryPoints :: UserId -> ServerT WebsiteSubRouteAPI (AppT)
websiteEntryPoints uId = getWebsitesEntryPoint uId
                    :<|> postWebsiteEntryPoint uId
                    :<|> postCredentialsEntryPoint uId
                    :<|> getCredentialsEntryPoint uId

getWebsitesEntryPoint :: ( MonadIO m
                         , DBQueryWebsite m
                         ) => UserId -> m [WebsiteDetails]
getWebsitesEntryPoint = getWebsites 

postWebsiteEntryPoint :: ( MonadIO m
                         , DBModifyWebsite m
                         ) => UserId -> PostWebsite -> m WebsiteId
postWebsiteEntryPoint uId (PostWebsite{postWebURL= pwu, postWebsiteName=pwn}) =
  insertWebsite uId pwu pwn

postCredentialsEntryPoint :: ( MonadIO m
                             , Crypto m
                             , DBModifyCredentials m
                             ) => UserId -> WebsiteId -> PostCredentials -> m ()
postCredentialsEntryPoint uid wid PostCredentials{ postWebUsername = un
                                                 , postWebPassword = pass
                                                 , postMasterKey = PostMasterKey key
                                                 } = do
  ep <- encrypt key pass
  _ <- insertCredentials uid wid ep un
  return ()



getCredentialsEntryPoint :: ( MonadIO m
                            , DBQueryWebsite m
                            , DBQueryCredentials m
                            , Crypto m
                            ) => UserId -> WebsiteId -> PostMasterKey -> m Website
getCredentialsEntryPoint uid wid (PostMasterKey key) = do
  creds <- getCredentials uid wid
  webdetails <- getWebsite uid wid
  decryptedCreds <- mapM decryptCreds creds
  return Website { websiteDetails = webdetails
                 , websiteCredentials = decryptedCreds
                 }

  where
    decryptCreds creds = do
      let epass = encPassword creds
          un = webUsername creds
      pass <- decrypt key epass
      return WebsiteCredentials { username = un
                                , password = pass
                                }
      
