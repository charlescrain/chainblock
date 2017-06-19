{-# LANGUAGE TypeOperators #-}

module ChainBlock.API where

import           Control.Monad.Error.Class      (MonadError)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Reader           (MonadReader, ReaderT,
                                                 runReaderT)
import           Servant                        (Handler, ServantErr)

import           ChainBlock.API.Interfaces
import           ChainBlock.API.Types
import qualified ChainBlock.Business            as BIZ
import qualified ChainBlock.Business.Interfaces as BIZ



routeInterface :: IO (IRouteFunctions IO Handler)
routeInterface = do
  return IRouteFunctions { getUsers = getUsers'
                         , postUser = postUser'
                         , getWebsites = getWebsites'
                         , getWebsiteCredentials  = getWebsiteCredentials'
                         , postWebsites = postWebsites'
                         , runRouteInterface = runRouteInterface'
                         }

-----------------------------------------------------
-- | Interface Implementation
-----------------------------------------------------

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

runRouteInterface' :: IO a -> Handler a
runRouteInterface' = undefined

