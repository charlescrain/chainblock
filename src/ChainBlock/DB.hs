{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module ChainBlock.DB where

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, runReaderT)
import           Data.ByteString.Lazy       (toStrict)
import           Data.ByteString.Lazy.UTF8  (fromString)
import           Database.PostgreSQL.Simple
import           System.Environment         (getEnv)

import           ChainBlock.DB.Interfaces
import           ChainBlock.DB.Setup        (createDBIfNeeded)
import           ChainBlock.DB.Types

-- type MonadDB =

databaseInterface :: IO (IDataBase IO IO)
databaseInterface = do
  connInfo <- buildConnectInfo
  conn <- connect connInfo
  let dbName = connectDatabase connInfo
  _ <- createDBIfNeeded conn dbName
  return IDataBase { queryAllUsers           = queryAllUsers' conn
                   , queryUser               = queryUser' conn
                   , insertUser              = insertUser' conn
                   , queryWebsite            = queryWebsite' conn
                   , queryWebsiteCredentials = queryWebsiteCredentials' conn
                   , runDBInterface          = runDBInterface'
                   }

-----------------------------------------------------
-- | Interface Implementation
-----------------------------------------------------

queryAllUsers' :: Connection -> IO [User]
queryAllUsers' =  undefined

queryUser' :: Connection -> UserId -> IO User
queryUser' = undefined

insertUser' ::  Connection -> Username -> IO UserId
insertUser' = undefined

queryWebsite' :: Connection -> UserId -> IO [Website]
queryWebsite' = undefined

queryWebsiteCredentials' :: Connection -> UserId -> WebsiteId -> IO [WebsiteCredentials]
queryWebsiteCredentials' = undefined

runDBInterface' :: IO a -> IO a
runDBInterface' = undefined

-----------------------------------------------------
-- | Helper Funcitons
-----------------------------------------------------


buildConnectInfo :: IO ConnectInfo
buildConnectInfo  = do
  host       <- getEnv "PG_HOST"
  port       <- getEnv "PG_PORT"
  dbName     <- getEnv "PG_DBNAME"
  dbUser     <- getEnv "PG_DBUSER"
  dbPassword <- getEnv "PG_DBPASSWORD"
  return $ ConnectInfo { connectHost = host
                       , connectPort = read port
                       , connectUser = dbUser
                       , connectPassword = dbPassword
                       , connectDatabase = dbName
                       }

