{-# LANGUAGE DeriveGeneric #-}

module Tholos.API.Types where

import           Data.Aeson
import           Data.Text       (Text)
import           GHC.Generics
import           Web.HttpApiData

-----------------------------------------------------
-- | User
-----------------------------------------------------

data User =
  User  { name :: Username
        , id   :: UserId
        }
  deriving (Show, Eq, Generic)
instance ToJSON User
instance FromJSON User


newtype Username = Username Text
  deriving (Show, Eq, Ord, Generic)
instance ToJSON Username
instance FromJSON Username

newtype UserId = UserId Integer
  deriving (Show, Eq, Ord, Generic)
instance ToJSON UserId
instance FromJSON UserId
instance ToHttpApiData UserId
instance FromHttpApiData UserId


-----------------------------------------------------
-- | Website
-----------------------------------------------------

data Website =
  Website { websiteDetails     :: WebsiteDetails
          , websiteCredentials :: [WebsiteCredentials]
          }
  deriving (Show, Eq, Ord, Generic)
instance ToJSON Website
instance FromJSON Website

data WebsiteDetails =
  WebsiteDetails { webURL      :: WebsiteURL
                 , websiteName :: Text
                 , websiteId   :: WebsiteId
                 , userId      :: UserId
                 }
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebsiteDetails
instance FromJSON WebsiteDetails

data WebsiteCredentials =
  WebsiteCredentials  { username :: WebUsername
                      , password :: WebPassword
                      }
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebsiteCredentials
instance FromJSON WebsiteCredentials

newtype WebPassword = WebPassword Text
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebPassword
instance FromJSON WebPassword

newtype WebUsername = WebUsername Text
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebUsername
instance FromJSON WebUsername

newtype WebsiteURL = WebsiteURL Text
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebsiteURL
instance FromJSON WebsiteURL

newtype WebsiteId = WebsiteId Integer
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebsiteId
instance FromJSON WebsiteId
instance ToHttpApiData WebsiteId
instance FromHttpApiData WebsiteId


-----------------------------------------------------
-- |POST Types
-----------------------------------------------------

newtype PostUserBody = PostUserBody Username
  deriving (Show, Eq, Generic)
instance ToJSON PostUserBody
instance FromJSON PostUserBody

data PostWebsite = PostWebsite { postWebURL      :: WebsiteURL
                               , postWebsiteName :: Text
                               }
  deriving (Show, Eq, Ord, Generic)
instance ToJSON PostWebsite
instance FromJSON PostWebsite

newtype PostMasterKey = PostMasterKey Text
  deriving (Show, Eq, Generic)
instance ToJSON PostMasterKey
instance FromJSON PostMasterKey

data PostCredentials = PostCredentials { postWebUsername :: WebUsername
                                       , postWebPassword :: WebPassword
                                       , postMasterKey   :: PostMasterKey
                                       }
  deriving (Show, Eq, Generic)
instance ToJSON PostCredentials
instance FromJSON PostCredentials
