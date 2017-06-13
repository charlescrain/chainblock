{-# LANGUAGE DeriveGeneric #-}

module ChainBlock.DB.Types where

import           Data.Text               (Text)
import           GHC.Generics

import           Data.Profunctor.Product (p2, p3, p4)
import           Opaleye                 (Column, PGInt4, PGText, Table (Table),
                                          optional, required, (.<), (.==))
-----------------------------------------------------
-- | User
-----------------------------------------------------

data User =
  User  { name :: Username
        , uId  :: UserId
        }
  deriving (Show, Eq, Generic)

userTable :: Table (Maybe (Column PGInt4), Column PGText)
                     (Column PGInt4, Column PGText)
userTable = Table "userTable" (p2 ( optional "id"
                                   , required "name" ))

newtype UserId = UserId {unUserId :: Text}
  deriving (Show, Eq, Ord, Generic)

newtype Username = Username {unUsername :: Text}
  deriving (Show, Eq, Ord, Generic)

-----------------------------------------------------
-- | Website
-----------------------------------------------------

data WebsiteCredentials =
  WebsiteCredentials  { username  :: WebUsername
                      , password  :: EncryptedPassword
                      , websiteId :: WebsiteId
                      }
  deriving (Show, Eq, Ord, Generic)

credentialsTable :: Table (Column PGText, Column PGText, Column PGInt4)
                          (Column PGText, Column PGText, Column PGInt4)
credentialsTable = Table "credentialsTable" (p3 ( required "username"
                                                , required "encrypted_pass"
                                                , required "website_id" ))

data Website =
  Website { webURL      :: WebsiteURL
          , websiteName :: Text
          , wId         :: WebsiteId
          , userId      :: UserId
          }
  deriving (Show, Eq, Ord, Generic)

websiteTable :: Table (Column PGInt4, Column PGText, Column PGText, Column PGInt4)
                      (Column PGInt4, Column PGText, Column PGText, Column PGInt4)
websiteTable = Table "credentialsTable" (p4 ( required "id"
                                            , required "website_url"
                                            , required "website_name"
                                            , required "user_id" ))

newtype WebsiteId = WebsiteId  {unWebsiteId :: Integer}
  deriving (Show, Eq, Ord, Generic)

newtype EncryptedPassword =
  EncryptedPassword {unEncryptedPassword :: Text}
  deriving (Show, Eq, Ord, Generic)

newtype WebUsername = WebUsername {unWebUsername :: Text}
  deriving (Show, Eq, Ord, Generic)

newtype WebsiteURL = WebsiteURL  {unWebsiteURL :: Text}
  deriving (Show, Eq, Ord, Generic)

