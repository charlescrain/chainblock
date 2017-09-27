module Config.AppConfig
  ( AppConfig(..)
  , getAppConfig
  , Environment (..)
  ) where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           System.Environment        (getEnv)

import           Config.Environment
import           Tholos.Business.Interface

data AppConfig m = AppConfig
  { appEnv            :: Environment
  , appPort           :: Int
  , appRouteInterface :: IBusinessFunctions m
  } --deriving (Show)




getAppConfig :: ( MonadIO m
                --, MonadError e m
                )
             => IBusinessFunctions m
             -> IO (AppConfig m)
getAppConfig irf = do
  env <- read <$> getEnv "ENV"
  port <- read <$> getEnv "PORT"
  return $ AppConfig env port irf
