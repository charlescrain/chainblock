{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module ChainBlock.Business where

import           Control.Monad.Error.Class      (MonadError)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Reader           (MonadReader, ReaderT,
                                                 runReaderT)

import           ChainBlock.Business.Interfaces
import           ChainBlock.Business.Types
import           ChainBlock.DB.Interfaces


businessInterface :: (IDataBase dbMonad BZ)
                  -> (forall a . BZ a -> m a )
                  -> IO (IBusinessFunctions BZ m)
businessInterface _ runBusinessInterface' = do
  return
    IBusinessFunctions { getUsers = getUsers'
                       , postUser = postUser'
                       , getWebsites = getWebsites'
                       , getWebsiteCredentials  = getWebsiteCredentials'
                       , postWebsites = postWebsites'
                       , runBusinessInterface = runBusinessInterface'
                       }

-----------------------------------------------------
-- | runBusinessInterface Functions
-----------------------------------------------------

runBusinessInterfaceIO :: BZ a -> IO a
runBusinessInterfaceIO = undefined

-----------------------------------------------------
-- | Interface Implementation
-----------------------------------------------------

getUsers' :: BZ [User]
getUsers' = undefined

postUser' :: PostUserBody -> BZ UserId
postUser' = undefined

getWebsites' :: UserId -> BZ [WebsiteDetails]
getWebsites' = undefined

getWebsiteCredentials' :: UserId
                       -> WebsiteId
                       -> PostMasterKey
                       -> BZ Website
getWebsiteCredentials' = undefined

postWebsites' :: UserId -> PostMasterKey -> BZ WebsiteId
postWebsites' = undefined


