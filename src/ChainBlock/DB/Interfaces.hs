{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module ChainBlock.DB.Interfaces where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Servant                   (Handler, ServantErr)

import           ChainBlock.DB.Types
import           ChainBlock.Errors         (CBErrors (..))


class (MonadError CBErrors m, MonadIO m) => DBMonad m
-- type DBMonad m = (MonadError CBErrors m, MonadIO m) => m



data IDataBase m m'  =
  IDataBase { queryAllUsers :: (DBMonad m) => m [User]
            , queryUser     :: (DBMonad m) => UserId -> m User
            , insertUser :: (DBMonad m) => Username -> m UserId
            , queryWebsite :: (DBMonad m) => UserId -> m [Website]
            , queryWebsiteCredentials :: (DBMonad m)
                                      => UserId
                                      -> WebsiteId
                                      -> m [WebsiteCredentials]
            , runDBInterface :: forall a . m a -> m' a
            }


