module Tholos.App.Config
  ( AppConfig(..)
  , getAppConfig
  , Environment (..)
  ) where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           System.Environment        (getEnv)

import           Tholos.App.Environment
import           Tholos.Business.Interface

data AppConfig = AppConfig
  { appEnv            :: Environment
  , appPort           :: Int
  } --deriving (Show)




getAppConfig :: IO AppConfig 
getAppConfig = do
  env <- read <$> getEnv "ENV"
  port <- read <$> getEnv "PORT"
  return $ AppConfig env port
