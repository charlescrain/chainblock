module ChainBlock.Errors where

import           Control.Monad.Catch       (Exception (..), SomeException (..))
import           Control.Monad.Error.Class (Error (..))
import           Data.Text                 (Text)

data CBError = DatabaseError Text Text DB_Errors
             | BusinessError Text Text BZ_Errors
             | APIError      Text Text API_Errors
             | Ex            SomeException
  deriving (Show)
data DB_Errors  = NoResults
                | DuplicateKeyViolation
                | NoRowsAltered
  deriving (Show)
data BZ_Errors  = BZERROR
  deriving (Show)
data API_Errors = APIERROR
  deriving (Show)

-- instance Error CBErrors


data CBException = DatabaseEx Text Text DBException
                 | BusinessEx Text Text BZException
                 | APIEx      Text Text APIException
  deriving (Show)
data DBException = NotUnique
  deriving (Show)
data BZException = BZException
  deriving (Show)
data APIException = APIException
  deriving (Show)

instance Exception CBException
