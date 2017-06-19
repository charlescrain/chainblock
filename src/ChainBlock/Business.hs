{-# LANGUAGE TypeOperators #-}

module ChainBlock.Business where

import           Control.Monad.Error.Class      (MonadError)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Reader           (MonadReader, ReaderT,
                                                 runReaderT)

import           ChainBlock.Business.Interfaces
import           ChainBlock.Business.Types
import           ChainBlock.DB
import           ChainBlock.DB.Interfaces



businessInterface :: IO (IBusinessFunctions IO IO)
businessInterface = do
  _dbInterface <- databaseInterface
  return IBusinessFunctions { getUsers = getUsers'
                            , postUser = postUser'
                            , getWebsites = getWebsites'
                            , getWebsiteCredentials  = getWebsiteCredentials'
                            , postWebsites = postWebsites'
                            , runBusinessInterface = runRouteInterface'
                            }

getUsers' :: IO [User]
getUsers' = undefined

postUser' :: PostUserBody -> IO UserId
postUser' = undefined

getWebsites' :: UserId -> IO [WebsiteDetails]
getWebsites' = undefined

getWebsiteCredentials' :: UserId
                       -> WebsiteId
                       -> PostMasterKey
                       -> IO Website
getWebsiteCredentials' = undefined

postWebsites' :: UserId -> PostMasterKey -> IO WebsiteId
postWebsites' = undefined

runRouteInterface' :: IO a -> IO a
runRouteInterface' = undefined

