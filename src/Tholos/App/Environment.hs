module Tholos.App.Environment
  ( Environment(..)
  ) where

data Environment = Development
                 | Test
                 | Production
  deriving (Show, Eq, Read)
