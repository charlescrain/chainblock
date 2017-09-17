{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ChainBlock.Business.Types
       ( module ChainBlock.API.Types
       , BZ (..)
       )
       where

import           Control.Monad.Catch     (MonadCatch, MonadThrow)
import           Control.Monad.Except    (ExceptT, MonadError)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Logger    (LoggingT, MonadLogger)
import           Control.Monad.Reader    (MonadReader, ReaderT, runReaderT)

import           ChainBlock.API.Types
import           ChainBlock.DB.Interface
import           ChainBlock.Errors


-- newtype BZ a = BZ { runBZ :: BZMonadStack a}
--     deriving ( Functor
--              , Applicative
--              , Monad
--              , MonadIO
--              , MonadCatch
--              , MonadLogger
--              , MonadError CBError
--              , MonadThrow
--              )
newtype BZ m a = BZ { runBZ :: (ReaderT (IDataBase m (BZ m)) (ExceptT CBError (LoggingT IO))) a}
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadCatch
             , MonadLogger
             , MonadError CBError
             , MonadThrow
             , MonadReader (IDataBase m (BZ m))
             )
