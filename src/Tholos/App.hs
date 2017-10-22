{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Tholos.App
  ( app
  , AppConfig(..)
  , getAppConfig
  ) where

import           Control.Monad.IO.Class               (MonadIO)
import           Data.Monoid                          ((<>))
import           Network.Wai                          as Wai
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant

import           Tholos.API
import           Tholos.App.Transformer
import           Tholos.AppConfig                     (AppConfig (..),
                                                       getAppConfig)
import           Tholos.Server                        (server)

app :: AppConfig -> Wai.Application
app cfg = logStdoutDev . cors (const $ Just corsPolicy) $
            serve api (readerServer cfg)

readerServer :: AppConfig -> Server API
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: AppConfig -> TholosT :~> Handler
readerToEither cfg = Nat $ \tholosT -> runTholosT cfg tholosT

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let allowedMethods = simpleMethods <> ["DELETE", "PUT", "PATCH", "OPTIONS"]
      allowedHeaders = ["Content-Type"]
  in
    simpleCorsResourcePolicy { corsMethods = allowedMethods
                             , corsRequestHeaders = allowedHeaders
                             }
