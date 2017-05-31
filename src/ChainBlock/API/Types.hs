{-# LANGUAGE DeriveGeneric #-}

module ChainBlock.API.Types where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics


data User =
  User  { name :: Text
        , id   :: Integer
        }
  deriving (Show, Eq, Generic)

instance ToJSON User

newtype Password = Password {unPassword :: Text}
  deriving (Show, Eq, Ord)

newtype Username = Username {unUsername :: Text}
  deriving (Show, Eq, Ord)

newtype WebsiteURL = WebsiteURL  {unWebsiteURL :: Text}
  deriving (Show, Eq, Ord)

data WebsiteCredentials =
  WebsiteCredentials  { username :: Username
                      , password :: Password
                      , webURL   :: WebsiteURL
                      }
  deriving (Show, Eq, Ord)
