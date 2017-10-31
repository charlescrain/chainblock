module Tholos.App.Config where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             close, connect)
import           System.Environment        (getEnv)

import           Tholos.App.Environment
import           Tholos.Business.Interface

data AppConfig = AppConfig
  { appEnv  :: Environment
  , appPort :: Int
  , conn    :: Connection
  } --deriving (Show)




mkAppConfig :: Connection -> IO AppConfig
mkAppConfig conn = do
  env <- read <$> getEnv "ENV"
  port <- read <$> getEnv "PORT"
  return $ AppConfig env port conn
