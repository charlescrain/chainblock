{-# LANGUAGE DeriveGeneric #-}

module ChainBlock.API.Types where

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

data PostUserBody = PostUserBody  { postUserBodyName :: Username }
  deriving (Show, Eq, Generic)
instance ToJSON PostUserBody
instance FromJSON PostUserBody

data PostMasterKey = PostMasterKey  { masterKey :: Text }
  deriving (Show, Eq, Generic)
instance ToJSON PostMasterKey
instance FromJSON PostMasterKey

newtype Username = Username {unUsername :: Text}
  deriving (Show, Eq, Ord, Generic)
instance ToJSON Username
instance FromJSON Username

newtype UserId = UserId {unUserId :: Text}
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

newtype WebPassword = WebPassword {unWebPassword :: Text}
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebPassword
instance FromJSON WebPassword

newtype WebUsername = WebUsername {unWebUsername :: Text}
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebUsername
instance FromJSON WebUsername

newtype WebsiteURL = WebsiteURL  {unWebsiteURL :: Text}
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebsiteURL
instance FromJSON WebsiteURL

newtype WebsiteId = WebsiteId  {unWebsiteId :: Integer}
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebsiteId
instance FromJSON WebsiteId
instance ToHttpApiData WebsiteId
instance FromHttpApiData WebsiteId



