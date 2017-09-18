{-# LANGUAGE DeriveGeneric #-}

module Tholos.Crypto.Types where

import           Data.ByteString (ByteString)
import           GHC.Generics

newtype MasterKey = MasterKey {mKey :: ByteString}
  deriving (Generic, Show , Eq)

newtype CipherText = CipherText {cTxt :: ByteString}
  deriving (Generic, Show, Eq)
