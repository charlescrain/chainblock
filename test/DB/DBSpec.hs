{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module DB.DBSpec (main, spec) where

import           Control.Monad.Except         (runExceptT)
import           Data.Either
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Test.Hspec

import           ChainBlock.DB.Interface
import           ChainBlock.DB.Postgres
import           ChainBlock.DB.Postgres.Setup
import           ChainBlock.DB.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  dbName <- runIO getEnv "PG_DBNAME"
  afterAll_ (dropDB dbName)
    dbSpec

dbSpec :: Spec
dbSpec =
    describe "DB Spec" $ do

      dbi <- runIO $ databaseInterface runDBInterfaceIO

      it "should create a user and query the user" $ do
        eRes <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
        isRight eRes `shouldBe` True
        -- pendingWith "Under construction"



------------------------------------------------------------------------------
--Spec Fixtures
------------------------------------------------------------------------------
testUsername :: Username
testUsername = Username "rob.machads"


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------


