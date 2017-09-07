{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module ChainBlock.DB.Interface where

import           Control.Monad.Catch       (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Logger      (MonadLogger)

import           ChainBlock.DB.Types
import           ChainBlock.Errors         (CBError (..))


class ( MonadError CBError m
      , MonadThrow m
      , MonadIO m
      , MonadCatch m
      , MonadLogger m
      ) => DBMonad m

data IDataBase m m'  =
  IDataBase { -- Users
              queryAllUsers :: (DBMonad m) => m [User]
            , queryUser     :: (DBMonad m) => Username             -> m User
            , insertUser    :: (DBMonad m) => Username             -> m UserId
            , updateUser    :: (DBMonad m) => UserId   -> Username -> m ()
            , deleteUser    :: (DBMonad m) => UserId               -> m ()

              -- Website
            , queryWebsites  :: (DBMonad m)
                            => UserId
                            -> m [Website]
            , queryWebsite  :: (DBMonad m)
                            => WebsiteId
                            -> m Website
            , insertWebsite :: (DBMonad m)
                            => UserId
                            -> WebsiteURL
                            -> WebsiteName
                            -> m WebsiteId
            , updateWebsite :: (DBMonad m)
                            => WebsiteId
                            -> WebsiteURL
                            -> WebsiteName
                            -> m ()
            , deleteWebsite :: (DBMonad m)
                            => WebsiteId
                            -> m ()

              -- Credentials
            , queryCredentials  :: (DBMonad m)
                                => UserId
                                -> WebsiteId
                                -> m [Credentials]
            , insertCredentials :: (DBMonad m)
                                => UserId
                                -> EncryptedPassword
                                -> WebUsername
                                -> m CredentialsId
            , updateCredentials :: (DBMonad m)
                                => CredentialsId
                                -> EncryptedPassword
                                -> WebUsername
                                -> m ()
            , deleteCredentials :: (DBMonad m)
                                => CredentialsId
                                -> m ()
              -- run function
            , runDBI :: forall a . m a -> m' a
            }


