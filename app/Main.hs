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
import           App                                  (AppConfig (..), app,
                                                       getAppConfig)
import           App.Transformer                      (AppT)
import           ChainBlock.Business
import           ChainBlock.DB
import           Server                               (server)

main :: IO ()
main = do
  dbInterface <- databaseInterface runDBInterfaceBZ
  bizInterface' <-
    businessInterface runBusinessInterfaceHandler dbInterface
  cfg <- getAppConfig bizInterface'
  run (appPort cfg) (app cfg)

