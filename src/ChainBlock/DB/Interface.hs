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
  IDataBase { queryAllUsers :: (DBMonad m) => m [User]
            , queryUser     :: (DBMonad m) => Username -> m User
            , insertUser :: (DBMonad m) => Username -> m UserId
            , queryWebsite :: (DBMonad m) => UserId -> m [Website]
            , queryWebsiteCredentials :: (DBMonad m)
                                      => UserId
                                      -> WebsiteId
                                      -> m [WebsiteCredentials]
            , runDBI :: forall a . m a -> m' a
            }


