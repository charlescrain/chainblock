{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tholos.API.Class where

import           Tholos.Types

class DBModifyUser m where
  insertUser    :: Username             -> m UserId
