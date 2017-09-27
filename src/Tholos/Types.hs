{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tholos.Types where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import           Data.Char                 (isAsciiLower, isAsciiUpper)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Aeson
import           Data.Word                 (Word64)
import           GHC.Generics
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Modifiers
import           Web.HttpApiData


import           Tholos.Errors


-----------------------------------------------------
-- | User
-----------------------------------------------------

data User =
  User  { name :: Username
        , uId  :: UserId
        }
  deriving (Show, Eq, Generic)
instance ToJSON User
instance FromJSON User
instance ToHttpApiData User
instance FromHttpApiData User
instance Arbitrary User where
  arbitrary = User <$> arbitrary
                   <*> arbitrary


newtype UserId = UserId Integer
  deriving (Show, Eq, Ord, Generic)
instance ToJSON UserId
instance FromJSON UserId
instance ToHttpApiData UserId
instance FromHttpApiData UserId
instance Arbitrary UserId where
  arbitrary = (UserId . fromIntegral) <$> (chooseAny :: Gen Word64)


newtype Username = Username Text
  deriving (Show, Eq, Ord, Generic)
instance ToJSON Username
instance FromJSON Username
instance Arbitrary Username where
  arbitrary = fmap Username (genTextOfSize 8)

-----------------------------------------------------
-- | Credentials
-----------------------------------------------------

data Credentials = Credentials { credId     :: CredentialsId
                               , webUsername   :: WebUsername
                               , encPassword   :: EncryptedPassword
                               , webId      :: WebsiteId
                               , credUserId :: UserId
                               }
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary Credentials where
  arbitrary = Credentials <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary


newtype CredentialsId = CredentialsId Integer
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary CredentialsId where
  arbitrary = (CredentialsId . fromIntegral) <$> (chooseAny :: Gen Word64)

newtype EncryptedPassword = EncryptedPassword ByteString
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary EncryptedPassword where
  arbitrary = fmap (EncryptedPassword . T.encodeUtf8 . T.pack) (mapM (\ _ -> arbitrary :: Gen Char) [1..16])

newtype PlainTextPassword = PlainTextPassword Text
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary PlainTextPassword where
  arbitrary = fmap PlainTextPassword (genTextOfSize 12)
instance ToJSON PlainTextPassword
instance FromJSON PlainTextPassword

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
instance Arbitrary Website where
  arbitrary = Website <$> arbitrary
                      <*> listOf1 arbitrary

data WebsiteDetails =
  WebsiteDetails { websiteURL  :: WebsiteURL
                 , websiteName :: WebsiteName
                 , websiteId   :: WebsiteId
                 , userId      :: UserId
                 }
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary WebsiteDetails where
  arbitrary = WebsiteDetails <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
instance ToJSON WebsiteDetails
instance FromJSON WebsiteDetails

newtype WebsiteId   = WebsiteId Integer
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary WebsiteId where
  arbitrary = (WebsiteId . fromIntegral) <$> (chooseAny :: Gen Word64)
instance ToJSON WebsiteId
instance FromJSON WebsiteId
instance ToHttpApiData WebsiteId
instance FromHttpApiData WebsiteId

newtype WebUsername = WebUsername Text
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary WebUsername where
  arbitrary = fmap WebUsername (genTextOfSize 12)
instance ToJSON WebUsername
instance FromJSON WebUsername

newtype WebsiteURL  = WebsiteURL Text
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary WebsiteURL where
  arbitrary = do
    partialUrl <- genTextOfSize 12
    return (WebsiteURL $ "http://" <> partialUrl <> ".com")
instance ToJSON WebsiteURL
instance FromJSON WebsiteURL

newtype WebsiteName = WebsiteName Text
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary WebsiteName where
  arbitrary = fmap WebsiteName (genTextOfSize 12)
instance ToJSON WebsiteName
instance FromJSON WebsiteName

data WebsiteCredentials =
  WebsiteCredentials  { username :: WebUsername
                      , password :: PlainTextPassword
                      }
  deriving (Show, Eq, Ord, Generic)
instance ToJSON WebsiteCredentials
instance FromJSON WebsiteCredentials
instance Arbitrary WebsiteCredentials where
  arbitrary = WebsiteCredentials <$> arbitrary
                                 <*> arbitrary 

genTextOfSize :: Int -> Gen Text
genTextOfSize size = T.pack <$> mapM genLetterOrNumber [1..size]
    where
      genLetterOrNumber _ = do
        ch <- arbitrary :: Gen Char
        if isAsciiLower ch || isAsciiUpper ch
             then return ch
             else genLetterOrNumber 1
