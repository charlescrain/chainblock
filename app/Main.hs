{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Monoid                          ((<>))
import           Network.Wai                          as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant

import           App                                  (AppConfig, AppT)
import qualified App                                  as App
import           Routing

main :: IO ()
main = do
  cfg <- App.getAppConfig
  run (App.appPort cfg) (app cfg)

app :: AppConfig -> Wai.Application
app cfg = logStdoutDev . cors (const $ Just corsPolicy) $
            serve api (readerServer cfg)



readerServer :: AppConfig -> Server API
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: AppConfig -> AppT :~> Handler
readerToEither cfg = Nat $ \appT -> App.runAppT cfg appT

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let allowedMethods = simpleMethods <> ["DELETE", "PUT", "PATCH", "OPTIONS"]
      allowedHeaders = ["Content-Type"]
  in
    simpleCorsResourcePolicy { corsMethods = allowedMethods
                             , corsRequestHeaders = allowedHeaders
                             }
