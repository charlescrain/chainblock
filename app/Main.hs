{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class               (MonadIO)
import           Data.Monoid                          ((<>))
import           Database.PostgreSQL.Simple           (ConnectInfo (..),
                                                       Connection, connect)
import           Network.Wai                          as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant

import           API
import           App                                  (AppConfig (..), app,
                                                       getAppConfig)
import           App.Transformer                      (AppT)
import           Server                               (server)
import qualified Tholos.Business                      as BZ
import qualified Tholos.DB.Postgres                   as DB
import           Tholos.DB.Postgres.Setup

main :: IO ()
main = do
  conn <- initDB
  dbI <- DB.createInterface conn DB.runInterfaceCommonT

  bzI <- BZ.createInterface (BZ.runInterfaceCommonT dbI)

  cfg <- getAppConfig bzI
  run (appPort cfg) (app cfg)

initDB :: IO Connection
initDB = do
  connInfo <- DB.buildConnectInfo
  let dbName = connectDatabase connInfo
  _ <- createDBIfNeeded connInfo dbName
  connect connInfo

