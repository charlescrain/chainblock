{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class               (MonadIO)
import           Data.Monoid                          ((<>))
import           Network.Wai                          as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant

import           API
import           App                                  (AppConfig, AppT)
import qualified App                                  as App
import           ChainBlock.DB
import           Server                               (server)

main :: IO ()
main = do
  cfg <- App.getAppConfig routeInterface
  run (App.appPort cfg) (app cfg)

app :: MonadIO m => AppConfig m -> Wai.Application
app cfg = logStdoutDev . cors (const $ Just corsPolicy) $
            serve api (readerServer cfg)



readerServer ::  MonadIO m => AppConfig m -> Server API
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: MonadIO m => AppConfig m -> (AppT m) :~> Handler
readerToEither cfg = Nat $ \appT -> App.runAppT cfg appT

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let allowedMethods = simpleMethods <> ["DELETE", "PUT", "PATCH", "OPTIONS"]
      allowedHeaders = ["Content-Type"]
  in
    simpleCorsResourcePolicy { corsMethods = allowedMethods
                             , corsRequestHeaders = allowedHeaders
                             }
