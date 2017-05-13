{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
  ( AppT
  , AppConfig (..)
  , getAppConfig
  , runAppT
  ) where

import Config.AppConfig (AppConfig (..), getAppConfig)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Servant (Handler, ServantErr)

-- NOTE: `Handler` is equivalent to `type Handler = ExceptT ServantErr IO
newtype AppT a = AppT { unApp :: ReaderT AppConfig Handler a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppConfig
           , MonadError ServantErr
           )

runAppT :: AppConfig -> AppT a -> Handler a
runAppT cfg appT = runReaderT (unApp appT) cfg
