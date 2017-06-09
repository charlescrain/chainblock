{-# LANGUAGE TypeOperators #-}

module ChainBlock.DB where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT)
import           Servant                   (Handler, ServantErr)

import           ChainBlock.Interfaces


routeInterface :: IRouteFunctions IO
routeInterface = undefined
