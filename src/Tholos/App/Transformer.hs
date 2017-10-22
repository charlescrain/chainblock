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

import           Tholos.AppConfig       (AppConfig)
import           Tholos.Errors
import           Tholos.Logging
import           Tholos.API.Class

newtype TholosT a = TholosT { unTholos :: ReaderT AppConfig (ExceptT TholosError (LoggingT IO))  a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppConfig
           , MonadError TholosError
           )


-- NOTE: `Handler` is equivalent to `type Handler = ExceptT ServantErr IO
runTholosT :: AppConfig -> TholosT a -> Handler a
runTholosT cfg tholosT = do
  eVal <- liftIO $ flip runLoggingT logMsg $ runExceptT (runReaderT (unTholos tholosT) cfg)
  case eVal of
    Left err -> tholosErrorToServantErr err
    Right x  -> undefined


tholosErrorToServantErr :: TholosError -> Handler a
tholosErrorToServantErr err = case err of
  DatabaseError src msg _ -> do
    throwError $ err500 {errBody="DatabaseError: " <> (pack $ show msg) }
  err -> throwError $ err500 {errBody="Error not Handled" <> (pack $ show err) }


instance DBModifyUser TholosT where
  insertUser = undefined

instance DBQueryUser TholosT where
  getUsers = undefined

instance DBModifyWebsite TholosT where
  insertWebsite = undefined
