module Config.AppConfig
  ( AppConfig(..)
  , getAppConfig
  , Environment (..)
  ) where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           System.Environment        (getEnv)

import           ChainBlock.Interfaces
import           Config.Environment

data AppConfig m = AppConfig
  { appEnv            :: Environment
  , appPort           :: Int
  , appRouteInterface :: IRouteFunctions m
  } --deriving (Show)




getAppConfig :: ( MonadIO m
                --, MonadError e m
                )
             => IRouteFunctions m
             -> IO (AppConfig m)
getAppConfig irf = do
  env <- read <$> getEnv "ENV"
  port <- read <$> getEnv "PORT"
  return $ AppConfig env port irf
