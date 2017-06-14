{-# LANGUAGE TypeOperators #-}

module ChainBlock.Business where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT)
import           Servant                   (Handler, ServantErr)

import           ChainBlock.API.Interfaces
import           ChainBlock.DB.Interfaces


routeInterface :: IO (IRouteFunctions IO Handler)
routeInterface = undefined
