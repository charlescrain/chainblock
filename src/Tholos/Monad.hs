{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Tholos.Monad where

import           Control.Monad.Catch       (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Except      (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Logger

import           Tholos.Errors             (TholosError (..))
import           Tholos.Logging


class ( MonadError TholosError m
      , MonadThrow m
      , MonadIO m
      , MonadCatch m
      , MonadLogger m
      ) => MonadTholos m

type CommonT = ExceptT TholosError (LoggingT IO)

runCommonT :: CommonT a -> IO (Either TholosError a)
runCommonT = flip runLoggingT logMsg . runExceptT 
