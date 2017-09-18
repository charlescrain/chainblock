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

import           Tholos.DB.Types
import           Tholos.Errors             (CBError (..))
import           Tholos.Monad              (MonadTholos)



data IDataBase m m'  =
  IDataBase { -- Users
              queryAllUsers :: (MonadTholos m) => m [User]
            , queryUser     :: (MonadTholos m) => Username             -> m User
            , insertUser    :: (MonadTholos m) => Username             -> m UserId
            , updateUser    :: (MonadTholos m) => UserId   -> Username -> m ()
            , deleteUser    :: (MonadTholos m) => UserId               -> m ()

              -- Website
            , queryWebsites :: (MonadTholos m)
                            => UserId
                            -> m [Website]
            , queryWebsite  :: (MonadTholos m)
                            => WebsiteId
                            -> m Website
            , insertWebsite :: (MonadTholos m)
                            => UserId
                            -> WebsiteURL
                            -> WebsiteName
                            -> m WebsiteId
            , updateWebsite :: (MonadTholos m)
                            => WebsiteId
                            -> WebsiteURL
                            -> WebsiteName
                            -> m ()
            , deleteWebsite :: (MonadTholos m)
                            => WebsiteId
                            -> m ()

              -- Credentials
            , queryAllUserCredentials  :: (MonadTholos m)
                                       => UserId
                                       -> m [Credentials]
            , queryCredentials  :: (MonadTholos m)
                                => CredentialsId
                                -> m Credentials
            , insertCredentials :: (MonadTholos m)
                                => UserId
                                -> WebsiteId
                                -> EncryptedPassword
                                -> WebUsername
                                -> m CredentialsId
            , updateCredentials :: (MonadTholos m)
                                => CredentialsId
                                -> EncryptedPassword
                                -> WebUsername
                                -> m ()
            , deleteCredentials :: (MonadTholos m)
                                => CredentialsId
                                -> m ()
              -- run function
            , runDBI :: (MonadTholos m, MonadTholos m')
                     => forall a . m a -> m' a
            }


