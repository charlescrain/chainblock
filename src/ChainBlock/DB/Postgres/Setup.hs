{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChainBlock.DB.Postgres.Setup
  ( createTables
  , createDBIfNeeded
  , dropDB
  ) where

import           Control.Monad                      (void)
import           Data.Binary.Builder
import           Data.ByteString.Char8              (pack)
import           Data.ByteString.Lazy               (fromStrict)
import           Data.Monoid                        ((<>))
import           Data.String                        (fromString)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Types

import           ChainBlock.DB.Postgres.Tables

createTables :: Connection -> IO ()
createTables conn = do
  _ <- begin conn
  _ <- mapM_ (execute_ conn) tableCreations
  commit conn
  where
    tableCreations = [ createUserTable
                     , createWebsiteTable
                     , createCredentialsTable
                     ]

dbExists :: Connection -> String -> IO Bool
dbExists conn dbName = do
  mDBCount <- query conn
    [sql| SELECT COUNT(*) FROM pg_database WHERE datname = ?; |]
    [dbName]
  case mDBCount of
    [Only (1 :: Integer)] -> return True
    _                     -> return False

-- TODO: Need to handle closing DBs.
createDBIfNeeded :: ConnectInfo -> String -> IO ()
createDBIfNeeded connInfo dbName = do
    conn <- connect connInfo {connectDatabase="postgres"}
    dbExists' <- dbExists conn dbName
    if dbExists' then return ()
    else do
      void $ createDB conn
      close conn
      conn' <- connect connInfo {connectDatabase=dbName}
      void $ createTables conn'
      close conn'
      return ()
    where
      createDB conn = do
        putStrLn $ "Creating Database " <> dbName
        void $ execute conn
                 [sql| CREATE DATABASE ?; |]
                 [Plain (fromLazyByteString . fromStrict . pack $ dbName)]

dropDB :: ConnectInfo -> IO ()
dropDB connInfo = do
  let dbName = connectDatabase connInfo
  conn <- connect connInfo {connectDatabase="postgres"}
  dbExists' <- dbExists conn dbName
  if not dbExists' then return ()
  else do
    putStrLn $ "Deleting Database " <> dbName
    void $ execute conn
             [sql| DROP DATABASE ?; |]
             [Plain (fromLazyByteString . fromStrict . pack $ dbName)]

