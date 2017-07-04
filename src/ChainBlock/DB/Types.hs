{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ChainBlock.DB.Types where

import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import           Control.Monad.Except   (ExceptT, MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)
import           GHC.Generics

import           ChainBlock.Errors

newtype PGDB a =
  PGDB { runPGDB :: (ExceptT CBErrors IO) a}
    deriving ( Functor, Applicative, Monad,
               MonadIO,
               MonadCatch,
               MonadError CBErrors,
               MonadThrow
             )

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

