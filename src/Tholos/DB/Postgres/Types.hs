{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tholos.DB.Postgres.Types where

import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import           Control.Monad.Except   (ExceptT, MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger   (LoggingT, MonadLogger)
import           Data.Text              (Text)
import           GHC.Generics

import           Tholos.DB.Interface
import           Tholos.Errors
import           Tholos.Monad


-----------------------------------------------------
-- | PGDB
-----------------------------------------------------

newtype PGDB a =
  PGDB { runPGDB :: CommonT a}
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadCatch
             , MonadLogger
             , MonadError TholosError
             , MonadThrow
             )
instance MonadTholos PGDB


