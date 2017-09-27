{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Tholos.Business where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Except      (ExceptT (..), runExceptT,
                                            throwError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT)
import           Servant                   (Handler)

import           Tholos.Business.Interface
import           Tholos.Business.Types
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
                       , postWebsites    = \x y -> runInterface $ postWebsites' x y
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

-- | Interface Implementation
-----------------------------------------------------

getUsers' :: BZ [User]
getUsers' = undefined

postUser' :: PostUserBody -> BZ UserId
postUser' = undefined

getWebsites' :: UserId -> BZ [WebsiteDetails]
getWebsites' = undefined

postWebsites' :: UserId -> PostWebsite -> BZ WebsiteId
postWebsites' = undefined

getCredentials' :: UserId
                -> WebsiteId
                -> PostMasterKey
                -> BZ Website
getCredentials' = undefined


postCredentials' :: UserId
                -> WebsiteId
                -> PostCredentials
                -> m ()
postCredentials' = undefined
