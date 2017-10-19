module Tholos.AppConfig.Environment
  ( Environment(..)
  ) where

data Environment = Development
                 | Test
                 | Production
  deriving (Show, Eq, Read)
