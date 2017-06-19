{-# LANGUAGE OverloadedStrings #-}

module DB.DBSpec (main, spec) where

import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import           Test.Hspec

import           App
import           ChainBlock.API.Types
import           ChainBlock.Crypto
import           ChainBlock.Interfaces
import           Config.Environment

main :: IO ()
main = hspec spec

spec :: Spec
spec = dbSpec

dbSpec :: Spec
dbSpec =
    describe "DB Spec" $ do
      dbi <- runIO databaseInterface
      it "should insert a User" $ \port -> do
        pendingWith "Underconstruction"


------------------------------------------------------------------------------
--Spec Fixtures
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------


databaseInterface :: IO (IDataBase IO IO)
databaseInterface = undefined
