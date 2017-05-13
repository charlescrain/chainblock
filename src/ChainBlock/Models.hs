module Models where

import Data.Text (Text)

data User =
  User  { username :: Text
        , id :: number
        }
