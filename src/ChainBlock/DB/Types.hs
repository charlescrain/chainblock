{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module ChainBlock.DB.Types where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import           Data.Char                 (isAsciiLower, isAsciiUpper)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Word                 (Word64)
import           GHC.Generics
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Modifiers


import           ChainBlock.Errors


-----------------------------------------------------
-- | User
-----------------------------------------------------

data User =
  User  { name :: Username
        , uId  :: UserId
        }
  deriving (Show, Eq, Generic)
instance Arbitrary User where
  arbitrary = User <$> arbitrary
                   <*> arbitrary


newtype UserId = UserId Integer
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary UserId where
  arbitrary = (UserId . fromIntegral) <$> (chooseAny :: Gen Word64)


newtype Username = Username Text
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary Username where
  arbitrary = fmap Username (genTextOfSize 8)

-----------------------------------------------------
-- | Website
-----------------------------------------------------

data Credentials = Credentials { credId     :: CredentialsId
                               , username   :: WebUsername
                               , password   :: EncryptedPassword
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

newtype CredentialsId = CredentialsId  {unCredentialsId :: Integer}
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary CredentialsId where
  arbitrary = (CredentialsId . fromIntegral) <$> (chooseAny :: Gen Word64)

newtype EncryptedPassword = EncryptedPassword ByteString
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary EncryptedPassword where
  arbitrary = fmap (EncryptedPassword . T.encodeUtf8 . T.pack) (mapM (\ _ -> arbitrary :: Gen Char) [1..16])


data Website =
  Website { websiteURL  :: WebsiteURL
          , websiteName :: WebsiteName
          , websiteId   :: WebsiteId
          , userId      :: UserId
          }
  deriving (Show, Eq, Ord, Generic)

newtype WebsiteId   = WebsiteId   {unWebsiteId   :: Integer}
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary WebsiteId where
  arbitrary = (WebsiteId . fromIntegral) <$> (chooseAny :: Gen Word64)

newtype WebUsername = WebUsername {unWebUsername :: Text}
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary WebUsername where
  arbitrary = fmap WebUsername (genTextOfSize 12)


newtype WebsiteURL  = WebsiteURL Text
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary WebsiteURL where
  arbitrary = do
    partialUrl <- genTextOfSize 12
    return (WebsiteURL $ "http://" <> partialUrl <> ".com")

newtype WebsiteName = WebsiteName Text
  deriving (Show, Eq, Ord, Generic)
instance Arbitrary WebsiteName where
  arbitrary = fmap WebsiteName (genTextOfSize 12)


genTextOfSize :: Int -> Gen Text
genTextOfSize size = T.pack <$> mapM genLetterOrNumber [1..size]
    where
      genLetterOrNumber _ = do
        ch <- arbitrary :: Gen Char
        if isAsciiLower ch || isAsciiUpper ch
             then return ch
             else genLetterOrNumber 1
