{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module ChainBlock.DB where

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT)

import           ChainBlock.DB.Interfaces
import           ChainBlock.DB.Setup       (createDBIfNeeded)
import           ChainBlock.DB.Types

databaseInterface :: IO (IDataBase IO IO)
databaseInterface = do
-- TODO: Discover Types for these functions
--   dbEnv <- getDBEnv
--   conn <- buildConnection dbEnv
--   _ <- createDBIfNeeded conn
  return IDataBase { queryAllUsers = queryAllUsers'
                   , queryUser = queryUser'
                   , insertUser = insertUser'
                   , queryWebsite = queryWebsite'
                   , queryWebsiteCredentials = queryWebsiteCredentials'
                   , runDBInterface = runDBInterface'
                   }
queryAllUsers' :: IO [User]
queryAllUsers' =  undefined

queryUser' :: UserId -> IO User
queryUser' = undefined

insertUser' ::  Username -> IO UserId
insertUser' = undefined

queryWebsite' :: UserId -> IO [Website]
queryWebsite' = undefined

queryWebsiteCredentials' :: UserId -> WebsiteId -> IO [WebsiteCredentials]
queryWebsiteCredentials' = undefined

runDBInterface' :: IO a -> IO a
runDBInterface' = undefined


-- TODO: Discover Types for these functions
-- getDBEnv :: IO String
-- getDBEnv = undefined
--
-- buildConnection ::
-- buildConnection dbEnv = undefined
--
-- createDBIfNeeded ::
-- createDBIfNeeded conn = undefined



