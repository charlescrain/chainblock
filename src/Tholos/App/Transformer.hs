{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Tholos.App.Transformer where

import           Control.Monad.Except
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import Data.Monoid ((<>))
import Data.ByteString.Lazy.Char8 (pack)
import           Servant

import           Tholos.App.Config       (AppConfig)
import           Tholos.Errors
import           Tholos.Logging
import           Tholos.API.Class

newtype AppT a = AppT { unApp :: ReaderT AppConfig (ExceptT TholosError (LoggingT IO))  a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppConfig
           , MonadError TholosError
           )


-- NOTE: `Handler` is equivalent to `type Handler = ExceptT ServantErr IO
runAppT :: AppConfig -> AppT a -> Handler a
runAppT cfg appT = do
  eVal <- liftIO $ flip runLoggingT logMsg $ runExceptT (runReaderT (unApp appT) cfg)
  case eVal of
    Left err -> tholosErrorToServantErr err
    Right x  -> undefined


tholosErrorToServantErr :: TholosError -> Handler a
tholosErrorToServantErr err = case err of
  DatabaseError src msg _ -> do
    throwError $ err500 {errBody="DatabaseError: " <> (pack $ show msg) }
  err -> throwError $ err500 {errBody="Error not Handled" <> (pack $ show err) }


instance DBModifyUser AppT where
  insertUser = undefined

instance DBQueryUser AppT where
  getUsers = undefined

instance DBModifyWebsite AppT where
  insertWebsite = undefined

instance DBQueryWebsite AppT where
  getWebsites = undefined
  getWebsite = undefined
  
instance DBModifyCredentials AppT where
  insertCredentials = undefined
  
instance DBQueryCredentials AppT where
  getCredentials = undefined

instance Crypto AppT where
  encrypt = undefined
  decrypt = undefined
