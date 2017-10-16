{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Tholos.Business where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Except      (ExceptT (..), runExceptT,
                                            throwError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader
import           Servant                   (Handler)
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
                       , postCredentials = \x y z -> runInterface $ postCredentials' x y z
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
  let mKey = MasterKey $ encodeUtf8 tKey
  userCreds <- liftCommonT $ queryAllUserCredentials dbi uid
  let credsForSite = filter ((==) wid . webId) userCreds
  undefined


postCredentials' :: UserId
                 -> WebsiteId
                 -> PostCredentials
                 -> m ()
postCredentials' = undefined
