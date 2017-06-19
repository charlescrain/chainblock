{-# LANGUAGE QuasiQuotes #-}
module ChainBlock.DB.Tables where

import           Data.Profunctor.Product          (p2, p3, p4)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Opaleye                          (Column, PGBytea, PGInt4,
                                                   PGText, Table (Table),
                                                   optional, required, (.<),
                                                   (.==))


-----------------------------------------------------
-- | User
-----------------------------------------------------

userTable :: Table (Maybe (Column PGInt4), Column PGText)
                     (Column PGInt4, Column PGText)
userTable = Table "userTable" (p2 ( optional "id"
                                   , required "name" ))

createUserTable :: Query
createUserTable = [sql|
  CREATE TABLE IF NOT EXISTS users (
      id                 serial PRIMARY KEY NOT NULL UNIQUE,
      name           text NOT NULL UNIQUE
  );
  |]

-----------------------------------------------------
-- | Website
-----------------------------------------------------

credentialsTable :: Table (Column PGText, Column PGBytea, Column PGInt4)
                          (Column PGText, Column PGBytea, Column PGInt4)
credentialsTable = Table "credentialsTable" (p3 ( required "username"
                                                , required "encrypted_pass"
                                                , required "website_id" ))
createCredentialsTable :: Query
createCredentialsTable = [sql|
  CREATE TABLE IF NOT EXISTS credentials (
    username     text  NOT NULL,
    encrypted_pass     bytea  NOT NULL,
    website_id   integer REFERENCES websites(id) NOT NULL
  );
  |]

websiteTable :: Table (Column PGInt4, Column PGText, Column PGText, Column PGInt4)
                      (Column PGInt4, Column PGText, Column PGText, Column PGInt4)
websiteTable = Table "credentialsTable" (p4 ( required "id"
                                            , required "website_url"
                                            , required "website_name"
                                            , required "user_id" ))

createWebsiteTable :: Query
createWebsiteTable = [sql|
  CREATE TABLE IF NOT EXISTS websites (
    id              serial PRIMARY KEY  NOT NULL UNIQUE,
    website_url     text  NOT NULL,
    website_name    text  NOT NULL,
    user_id         integer REFERENCES users(id) NOT NULL
  );
  |]