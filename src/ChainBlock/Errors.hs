module ChainBlock.Errors where

import           Control.Exception.Base (SomeException)

data CBErrors = Database DB_Errors
              | Business BZ_Errors
              | API      API_Errors
              | Ex       SomeException

data DB_Errors  = DBERROR
data BZ_Errors  = BZERROR
data API_Errors = APIERROR

