{-# LANGUAGE DeriveGeneric #-}

module ChainBlock.DB.Types where

import           Data.Text    (Text)
import           GHC.Generics

-----------------------------------------------------
-- | User
-----------------------------------------------------

data User =
  User  { name :: Username
        , uId  :: UserId
        }
  deriving (Show, Eq, Generic)

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

data Website =
  Website { webURL      :: WebsiteURL
          , websiteName :: Text
          , wId         :: WebsiteId
          , userId      :: UserId
          }
  deriving (Show, Eq, Ord, Generic)

newtype WebsiteId = WebsiteId  {unWebsiteId :: Integer}
  deriving (Show, Eq, Ord, Generic)

newtype EncryptedPassword =
  EncryptedPassword {unEncryptedPassword :: Text}
  deriving (Show, Eq, Ord, Generic)

newtype WebUsername = WebUsername {unWebUsername :: Text}
  deriving (Show, Eq, Ord, Generic)

newtype WebsiteURL = WebsiteURL  {unWebsiteURL :: Text}
  deriving (Show, Eq, Ord, Generic)

