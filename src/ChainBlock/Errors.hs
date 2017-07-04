module ChainBlock.Errors where

import           Control.Monad.Catch       (Exception (..), SomeException (..))
import           Control.Monad.Error.Class (Error (..))
import           Data.Text                 (Text)

data CBErrors = DatabaseError Text Text DB_Errors
              | BusinessError Text Text BZ_Errors
              | APIError      Text Text API_Errors
  deriving (Show, Eq)
data DB_Errors  = NoResults
  deriving (Show, Eq)
data BZ_Errors  = BZERROR
  deriving (Show, Eq)
data API_Errors = APIERROR
  deriving (Show, Eq)

-- instance Error CBErrors


data CBExceptions = DatabaseEx Text Text DBException
                  | BusinessEx Text Text BZException
                  | APIEx      Text Text APIException
  deriving (Show, Eq)
data DBException = NotUnique
  deriving (Show, Eq)
data BZException = BZException
  deriving (Show, Eq)
data APIException = APIException
  deriving (Show, Eq)

instance Exception CBExceptions
