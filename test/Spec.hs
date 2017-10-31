{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec

import qualified API.Spec as API
import qualified Server.Spec as SVR
import qualified DB.Postgres.Spec as DB

main :: IO ()
main = hspec $ do
  API.spec
  DB.spec
  SVR.spec
  -- beforeAll setup $ do
  --   Addresses.spec
  --   Pragma.spec
  --   Contracts.spec
  --   Search.spec
  --   Users.spec
  --   E2E.spec
