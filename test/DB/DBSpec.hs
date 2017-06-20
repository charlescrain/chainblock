{-# LANGUAGE OverloadedStrings #-}

module DB.DBSpec (main, spec) where

import           Data.Monoid   ((<>))
import           Data.Text     (Text)
import           Test.Hspec

import           ChainBlock.DB
-- import           ChainBlock.DB.Setup

main :: IO ()
main = hspec spec

spec :: Spec
spec = dbSpec

dbSpec :: Spec
dbSpec =
    describe "DB Spec" $ do
      _dbi <- runIO databaseInterface
      it "should insert a User" $ do
        pendingWith "Underconstruction"


------------------------------------------------------------------------------
--Spec Fixtures
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------


