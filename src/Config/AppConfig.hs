module Config.AppConfig
  ( AppConfig(..)
  , getAppConfig
  ) where

import           System.Environment (getEnv)

import           Config.Environment

data AppConfig = AppConfig
  { appEnv  :: Environment
  , appPort :: Int
  } deriving (Show)

getAppConfig :: IO AppConfig
getAppConfig = do
  env <- read <$> getEnv "ENV"
  port <- read <$> getEnv "PORT"
  return $ AppConfig env port
