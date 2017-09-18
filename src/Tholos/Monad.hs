{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Tholos.Monad (MonadTholos) where

import           Control.Monad.Catch       (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Logger      (MonadLogger)

import           Tholos.Errors             (CBError (..))


class ( MonadError CBError m
      , MonadThrow m
      , MonadIO m
      , MonadCatch m
      , MonadLogger m
      ) => MonadTholos m
