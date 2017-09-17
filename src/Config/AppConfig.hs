module Config.AppConfig
  ( AppConfig(..)
  , getAppConfig
  , Environment (..)
  ) where

import           Control.Monad.Error.Class     (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
import           System.Environment            (getEnv)

import           ChainBlock.Business.Interface
import           Config.Environment

data AppConfig m m' = AppConfig
  { appEnv            :: Environment
  , appPort           :: Int
  , appRouteInterface :: IBusinessFunctions m m'
  } --deriving (Show)




getAppConfig :: ( MonadIO m
                --, MonadError e m
                )
             => IBusinessFunctions m m'
             -> IO (AppConfig m m')
getAppConfig irf = do
  env <- read <$> getEnv "ENV"
  port <- read <$> getEnv "PORT"
  return $ AppConfig env port irf
