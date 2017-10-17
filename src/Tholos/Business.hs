{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Tholos.Business where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Except      (ExceptT (..), runExceptT,
                                            throwError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Data.Monoid                ((<>))
import           Data.Text                  hiding (length, map, filter)
import           Servant                   (Handler)
import Crypto.Error (CryptoFailable(..), CryptoError(..))
import Data.Text.Encoding (encodeUtf8)

import           Tholos.Business.Interface
import           Tholos.Business.Types
import           Tholos.Crypto
import           Tholos.Crypto.Types
import           Tholos.Types
import           Tholos.DB.Interface
import           Tholos.DB.Postgres
import           Tholos.Errors
import           Tholos.Logging
import           Tholos.Monad


createInterface :: (forall a . BZ a -> m a )
                  -> IO (IBusinessFunctions m)
createInterface runInterface =
  return
    IBusinessFunctions { getUsers = runInterface getUsers'
                       , postUser = runInterface . postUser'

                       , getWebsites     = runInterface . getWebsites'
                       , postWebsite    = \x y -> runInterface $ postWebsites' x y
                       , getCredentials  = \x y z -> runInterface $ getCredentials' x y z
                       , postCredentials = \w x y z -> runInterface $ postCredentials' w x y z
                       }

-----------------------------------------------------
-- | runBusinessInterface Functions
-----------------------------------------------------

runInterfaceExceptT :: IDataBase CommonT -> BZ a -> (ExceptT CBError IO) a
runInterfaceExceptT = undefined

runInterfaceCommonT :: IDataBase CommonT -> BZ a -> CommonT a
runInterfaceCommonT dbi f = runReaderT (runBZ f) dbi

-----------------------------------------------------
-- | Interface Implementation
-----------------------------------------------------

getUsers' :: BZ [User]
getUsers' = do
  dbi <- ask
  liftCommonT $ queryAllUsers dbi

postUser' :: PostUserBody -> BZ UserId
postUser' (PostUserBody un) = do
  dbi <- ask
  liftCommonT $ insertUser dbi un

getWebsites' :: UserId -> BZ [WebsiteDetails]
getWebsites' uid = do 
  dbi <- ask
  liftCommonT $ queryWebsites dbi uid

postWebsites' :: UserId -> PostWebsite -> BZ WebsiteId
postWebsites' uid PostWebsite{postWebURL=url, postWebsiteName=name} = do 
  dbi <- ask
  liftCommonT $ insertWebsite dbi uid url name

getCredentials' :: UserId
                -> WebsiteId
                -> PostMasterKey
                -> BZ Website
getCredentials' uid wid (PostMasterKey tKey) = do
  dbi <- ask
  let mkey = masterKey tKey
  userCreds <- liftCommonT $ queryAllUserCredentials dbi uid
  let credsForSite = filter ((==) wid . webId) userCreds
  webCreds <- sequence $ map (credentialsToWebCredentials mkey) credsForSite
  wDetails <- liftCommonT $ queryWebsite dbi wid
  return Website { websiteDetails = wDetails
                 , websiteCredentials = webCreds
                 }
    where
      credentialsToWebCredentials mkey Credentials { webUsername = wun@(WebUsername twun)
                                                   , encPassword = EncryptedPassword ep
                                                   } = do
        let src = "getCredentials'"
            cfpass =  decryptWithMasterKey mkey (CipherText ep)
        case cfpass of
          CryptoFailed err -> do
            let errorMsg = "CryptoError with userId,webId,username"
                        <> pack (show uid)
                        <> ","
                        <> pack (show wid)
                        <> ","
                        <> twun
                        <> " error is: "
                        <> pack (show err)
            $logWarnS src errorMsg
            throwError $ BusinessError src errorMsg (Crypto err)
          CryptoPassed pass -> return WebsiteCredentials { username = wun
                                                         , password = PlainTextPassword pass
                                                         }


postCredentials' :: UserId
                 -> WebsiteId
                 -> PostMasterKey
                 -> PostCredentials
                 -> m ()
postCredentials' = undefined
