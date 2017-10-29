{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tholos.API.Class where

import           Tholos.Types
import           Tholos.Crypto.Types
import Data.Text (Text)

class DBModifyUser m where
  insertUser :: Username -> m UserId

class DBQueryUser m where
  getUsers :: m [User]

class DBModifyWebsite m where
  insertWebsite :: UserId -> WebsiteURL -> WebsiteName -> m WebsiteId

class DBQueryWebsite m where
  getWebsites :: UserId -> m [WebsiteDetails]
  getWebsite :: UserId -> WebsiteId -> m WebsiteDetails

class DBModifyCredentials m where
  insertCredentials :: UserId -> WebsiteId -> EncryptedPassword -> WebUsername -> m CredentialsId

class DBQueryCredentials m where
  getCredentials :: UserId -> WebsiteId -> m [Credentials]

class Crypto m where
  encrypt :: Text -> PlainTextPassword -> m EncryptedPassword
  decrypt :: Text -> EncryptedPassword -> m PlainTextPassword

