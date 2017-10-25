{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tholos.API.Class where

import           Tholos.Types
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
  getCredentials :: UserId -> WebsiteId -> m [(WebUsername, EncryptedPassword)]

class Encrypt m where
  encrypt :: Text -> PlainTextPassword -> m EncryptedPassword

class Decrypt m where
  decrypt :: Text -> PlainTextPassword -> m EncryptedPassword
