{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ChainBlock.DB.Types where

import           Data.ByteString   (ByteString)
import           Data.Text         (Text)
import           GHC.Generics

import           ChainBlock.Errors


-----------------------------------------------------
-- | User
-----------------------------------------------------

data User =
  User  { name :: Username
        , uId  :: UserId
        }
  deriving (Show, Eq, Generic)


newtype UserId = UserId {unUserId :: Integer}
  deriving (Show, Eq, Ord, Generic)

newtype Username = Username {unUsername :: Text}
  deriving (Show, Eq, Ord, Generic)

-----------------------------------------------------
-- | Website
-----------------------------------------------------

data Credentials = Credentials { credId    :: CredentialsId
                               , username  :: WebUsername
                               , password  :: EncryptedPassword
                               , websiteId :: WebsiteId
                               }
  deriving (Show, Eq, Ord, Generic)

newtype CredentialsId = CredentialsId  {unCredentialsId :: Integer}
  deriving (Show, Eq, Ord, Generic)

newtype EncryptedPassword =
  EncryptedPassword {unEncryptedPassword :: ByteString}
  deriving (Show, Eq, Ord, Generic)

data Website =
  Website { webURL      :: WebsiteURL
          , websiteName :: WebsiteName
          , wId         :: WebsiteId
          , userId      :: UserId
          }
  deriving (Show, Eq, Ord, Generic)

newtype WebsiteId   = WebsiteId   {unWebsiteId   :: Integer}
  deriving (Show, Eq, Ord, Generic)

newtype WebUsername = WebUsername {unWebUsername :: Text}
  deriving (Show, Eq, Ord, Generic)

newtype WebsiteURL  = WebsiteURL  {unWebsiteURL  :: Text}
  deriving (Show, Eq, Ord, Generic)

newtype WebsiteName = WebsiteName {unWebsiteName  :: Text}
  deriving (Show, Eq, Ord, Generic)
