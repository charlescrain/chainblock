{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ChainBlock.Business.Types
       ( module ChainBlock.API.Types
       , BZ (..)
       )
       where

import           Control.Monad.Catch       (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT)

import           ChainBlock.API.Types

newtype BZ a = BZ { runBZ :: IO a}
    deriving ( Functor, Applicative, Monad,
               MonadIO,
               MonadCatch,
               MonadThrow
             )
