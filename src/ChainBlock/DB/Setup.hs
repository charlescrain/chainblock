{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChainBlock.DB.Setup
  ( createTables,
    createDBIfNeeded
  ) where

import           Control.Monad                    (void)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

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

createUserTable :: Query
createUserTable = [sql|
  CREATE TABLE IF NOT EXISTS users (
      id                 serial PRIMARY KEY NOT NULL UNIQUE,
      username           text NOT NULL UNIQUE
  );
  |]

createWebsiteTable :: Query
createWebsiteTable = [sql|
  CREATE TABLE IF NOT EXISTS websites (
    id              serial PRIMARY KEY  NOT NULL UNIQUE,
    website_url     text  NOT NULL,
    website_name    text  NOT NULL,
    user_id         integer REFERENCES users(id) NOT NULL
  );
  |]

createCredentialsTable :: Query
createCredentialsTable = [sql|
  CREATE TABLE IF NOT EXISTS credentials (
    username     text  NOT NULL,
    password     bytea  NOT NULL,
    website_id   integer REFERENCES websites(id) NOT NULL
  );
  |]

