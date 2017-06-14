{-# LANGUAGE TypeOperators #-}

module ChainBlock.DB where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT)

import           ChainBlock.DB.Interfaces
import           ChainBlock.DB.Setup       (createDBIfNeeded)

databaseInterface :: IO (IDataBase IO IO)
databaseInterface = undefined
