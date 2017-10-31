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

import           Tholos.API
import           Tholos.App
import           Tholos.App.Config
import           Tholos.App.Transformer               (AppT)
import qualified Tholos.DB.Postgres                   as DB
import           Tholos.DB.Postgres.Setup
import           Tholos.Server                        (server)

main :: IO ()
main = do
  conn <- initDB

  cfg <- mkAppConfig conn
  run (appPort cfg) (app cfg)

initDB :: IO Connection
initDB = do
  connInfo <- DB.buildConnectInfo
  let dbName = connectDatabase connInfo
  _ <- createDBIfNeeded connInfo dbName
  connect connInfo

