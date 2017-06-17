{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChainBlock.DB.Setup
  ( createTables,
    createDBIfNeeded
  ) where

import           Control.Monad                    (void)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

import           ChainBlock.DB.Tables

createTables :: Connection -> IO ()
createTables conn = do
  _ <- begin conn
  _ <- mapM_ (execute_ conn) tableCreations
  commit conn
  where
    tableCreations = [ createUserTable
                     , createCredentialsTable
                     , createWebsiteTable
                     ]

createDBIfNeeded :: Connection -> IO ()
createDBIfNeeded conn = do
    dbExists' <- dbExists
    if dbExists' then return ()
    else createDB
    where
      dbExists = do
        mDBCount <- query_ conn
          [sql| SELECT COUNT(*) FROM pg_database WHERE datname = 'chainblock'; |]
        case mDBCount of
          [Only (1 :: Integer)] -> return False
          _                     -> return True
      createDB = do
        putStrLn "Creating Database chainblock"
        void $ execute_ conn [sql| CREATE DATABASE chainblock; |]



