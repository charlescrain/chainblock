{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Tholos.DB.Interface where

import           Control.Monad.Catch       (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Logger      (MonadLogger)

import           Tholos.Types
import           Tholos.Errors             (CBError (..))



data IDataBase m  =
  IDataBase { -- Users
              queryAllUsers :: m [User]
            , queryUser     :: Username             -> m User
            , insertUser    :: Username             -> m UserId
            , updateUser    :: UserId   -> Username -> m ()
            , deleteUser    :: UserId               -> m ()

              -- Website
            , queryWebsites :: UserId
                            -> m [WebsiteDetails]
            , queryWebsite  :: WebsiteId
                            -> m WebsiteDetails
            , insertWebsite :: UserId
                            -> WebsiteURL
                            -> WebsiteName
                            -> m WebsiteId
            , updateWebsite :: WebsiteId
                            -> WebsiteURL
                            -> WebsiteName
                            -> m ()
            , deleteWebsite :: WebsiteId
                            -> m ()

              -- Credentials
            , queryAllUserCredentials  :: UserId
                                       -> m [Credentials]
            , queryCredentials  :: CredentialsId
                                -> m Credentials
            , insertCredentials :: UserId
                                -> WebsiteId
                                -> EncryptedPassword
                                -> WebUsername
                                -> m CredentialsId
            , updateCredentials :: CredentialsId
                                -> EncryptedPassword
                                -> WebUsername
                                -> m ()
            , deleteCredentials :: CredentialsId
                                -> m ()
            }


