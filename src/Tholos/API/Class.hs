{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tholos.API.Class where

import           Tholos.Types

class DBModifyUser m where
  insertUser :: Username -> m UserId

class DBQueryUser m where
  getUsers :: m [User]

class DBModifyWebsite m where
  insertWebsite :: UserId -> WebsiteURL -> WebsiteName -> m WebsiteId
