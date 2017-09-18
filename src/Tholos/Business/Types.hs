{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Tholos.Business.Types
       ( module Tholos.API.Types
       , BZ (..)
       )
       where

import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import           Control.Monad.Except   (ExceptT, MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger   (LoggingT, MonadLogger)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)

import           Tholos.API.Types
import           Tholos.DB.Interface
import           Tholos.Errors
import           Tholos.Monad


newtype BZ a = BZ { runBZ :: (ExceptT CBError (LoggingT IO)) a}
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadCatch
             , MonadLogger
             , MonadError CBError
             , MonadThrow
             )
instance MonadTholos BZ
-- newtype BZ m a =
--   BZ { runBZ :: MonadTholos m => (ReaderT (IDataBase m (BZ m)) (ExceptT CBError (LoggingT IO))) a}
--     deriving ( Functor
--              , Applicative
--              , Monad
--              , MonadIO
--              , MonadCatch
--              , MonadLogger
--              , MonadError CBError
--              , MonadThrow
--              , MonadReader (IDataBase m (BZ m))
--              )
