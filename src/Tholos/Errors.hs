{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tholos.Errors where

import           Control.Monad.Catch       (Exception (..), SomeException (..))
import           Control.Monad.Error.Class (Error (..))
import           Data.Text                 (Text)
import Crypto.Error (CryptoError)

data TholosError = DatabaseError Text Text DB_Errors
                 | Crypto Text Text CryptoError
                 | Ex            SomeException
  deriving (Show)

data DB_Errors  = NoResults
                | DuplicateKeyViolation
                | NoRowsAltered
                | NotUnique'
  deriving (Show)

-- instance Error CBErrors

data TholosException = DatabaseEx Text Text DBException
  deriving (Show)
data DBException = NotUnique
  deriving (Show)
data BZException = BZException
  deriving (Show)
data APIException = APIException
  deriving (Show)

instance Exception TholosException
