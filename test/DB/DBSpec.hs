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
      _dbi <- runIO $ databaseInterface runDBInterfaceIO
      it "should create a user and query the user" $ do
        pendingWith "Underconstruction"


------------------------------------------------------------------------------
--Spec Fixtures
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------


