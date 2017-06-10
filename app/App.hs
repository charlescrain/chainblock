{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App
  ( app
  , AppConfig(..)
  , getAppConfig
  ) where

import           Control.Monad.IO.Class               (MonadIO)
import           Data.Monoid                          ((<>))
import           Network.Wai                          as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant

import           API
import           App.Transformer
import           ChainBlock.DB
import           Config.AppConfig                     (AppConfig (..),
                                                       getAppConfig)
import           Server                               (server)

app :: MonadIO m => AppConfig m m' -> Wai.Application
app cfg = logStdoutDev . cors (const $ Just corsPolicy) $
            serve api (readerServer cfg)

readerServer ::  MonadIO m => AppConfig m m' -> Server API
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: MonadIO m => AppConfig m m' -> (AppT m m') :~> Handler
readerToEither cfg = Nat $ \appT -> runAppT cfg appT

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let allowedMethods = simpleMethods <> ["DELETE", "PUT", "PATCH", "OPTIONS"]
      allowedHeaders = ["Content-Type"]
  in
    simpleCorsResourcePolicy { corsMethods = allowedMethods
                             , corsRequestHeaders = allowedHeaders
                             }
