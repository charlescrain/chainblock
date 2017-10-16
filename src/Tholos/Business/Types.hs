{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Tholos.Business.Types
       ( module Tholos.API.Types
       , BZ (..)
       , liftCommonT
       )
       where

import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import           Control.Monad.Except   (ExceptT, MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger   (LoggingT, MonadLogger)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Class

import           Tholos.API.Types
import           Tholos.DB.Interface
import           Tholos.Errors
import           Tholos.Monad


-- newtype BZ a = BZ { runBZ :: (ExceptT CBError (LoggingT IO)) a}
--     deriving ( Functor
--              , Applicative
--              , Monad
--              , MonadIO
--              , MonadCatch
--              , MonadLogger
--              , MonadError CBError
--              , MonadThrow
--              )
-- instance MonadTholos BZ
newtype BZ a =
  BZ { runBZ :: (ReaderT (IDataBase CommonT) CommonT) a}
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadCatch
             , MonadLogger
             , MonadError CBError
             , MonadThrow
             , MonadReader (IDataBase CommonT)
             )

liftCommonT :: CommonT a -> BZ a
liftCommonT = BZ . lift
-- instance Applicative (BZ m ) where
--   pure = undefined
