{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ChainBlock.DB.Postgres.Types where

import           Control.Monad.Catch     (MonadCatch, MonadThrow)
import           Control.Monad.Except    (ExceptT, MonadError)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Logger    (LoggingT, MonadLogger)
import           Data.Text               (Text)
import           GHC.Generics

import           ChainBlock.DB.Interface
import           ChainBlock.Errors


-----------------------------------------------------
-- | PGDB
-----------------------------------------------------

newtype PGDB a =
  PGDB { runPGDB :: (ExceptT CBError (LoggingT IO)) a}
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadCatch
             , MonadLogger
             , MonadError CBError
             , MonadThrow
             )

instance DBMonad PGDB


