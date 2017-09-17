{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module ChainBlock.Business where

import           Control.Monad.Error.Class     (MonadError)
import           Control.Monad.Except          (ExceptT (..), runExceptT,
                                                throwError)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Reader          (MonadReader, ReaderT,
                                                runReaderT)
import           Servant                       (Handler)

import           ChainBlock.Business.Interface
import           ChainBlock.Business.Types
import           ChainBlock.DB.Interface
import           ChainBlock.Errors
import           ChainBlock.Logging


businessInterface :: (forall a . BZ a -> m a )
                  -> IDataBase dbMonad BZ
                  -> IO (IBusinessFunctions BZ m)
businessInterface runBusinessInterface' _ =
  return
    IBusinessFunctions { getUsers = getUsers'
                       , postUser = postUser'

                       , getWebsites     = getWebsites'
                       , postWebsites    = postWebsites'
                       , getCredentials  = getCredentials'
                       , postCredentials = postCredentials'

                       , runBusinessInterface = runBusinessInterface'
                       }

-----------------------------------------------------
-- | runBusinessInterface Functions
-----------------------------------------------------

runBusinessInterfaceIO :: BZ a -> (ExceptT CBError IO) a
runBusinessInterfaceIO = undefined

runBusinessInterfaceHandler :: BZ a -> Handler a
runBusinessInterfaceHandler = undefined

-----------------------------------------------------
-- | runDBInterface Functions
-----------------------------------------------------

runPGDBInterfaceBZ :: PGDB a -> BZ a
runPGDBInterfaceBZ = undefined

-----------------------------------------------------
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
