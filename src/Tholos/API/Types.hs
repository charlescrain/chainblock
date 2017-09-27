{-# LANGUAGE DeriveGeneric #-}

module Tholos.API.Types where

import           Data.Aeson
import           Data.Text       (Text)
import           GHC.Generics
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Web.HttpApiData

import Tholos.Types

-----------------------------------------------------
-- |POST Types
-----------------------------------------------------

newtype PostUserBody = PostUserBody Username
  deriving (Show, Eq, Generic)
instance ToJSON PostUserBody
instance FromJSON PostUserBody
instance Arbitrary PostUserBody where
  arbitrary = PostUserBody <$> arbitrary

data PostWebsite = PostWebsite { postWebURL      :: WebsiteURL
                               , postWebsiteName :: Text
                               }
  deriving (Show, Eq, Ord, Generic)
instance ToJSON PostWebsite
instance FromJSON PostWebsite
instance Arbitrary PostWebsite where
  arbitrary = PostWebsite <$> arbitrary
                          <*> (genTextOfSize 12)

newtype PostMasterKey = PostMasterKey Text
  deriving (Show, Eq, Generic)
instance ToJSON PostMasterKey
instance FromJSON PostMasterKey
instance Arbitrary PostMasterKey where
  arbitrary = PostMasterKey <$> (genTextOfSize 12)

data PostCredentials = PostCredentials { postWebUsername :: WebUsername
                                       , postWebPassword :: PlainTextPassword
                                       , postMasterKey   :: PostMasterKey
                                       }
  deriving (Show, Eq, Generic)
instance ToJSON PostCredentials
instance FromJSON PostCredentials
instance Arbitrary PostCredentials where
  arbitrary = PostCredentials <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
