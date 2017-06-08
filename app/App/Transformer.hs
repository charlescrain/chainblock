{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Transformer where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT)
import           Servant                   (Handler, ServantErr)

import           Config.AppConfig          (AppConfig)

-- NOTE: `Handler` is equivalent to `type Handler = ExceptT ServantErr IO
newtype AppT m a = AppT { unApp :: ReaderT (AppConfig m) Handler a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (AppConfig m)
           , MonadError ServantErr
           )

runAppT :: AppConfig m -> AppT m a -> Handler a
runAppT cfg appT = runReaderT (unApp appT) cfg
