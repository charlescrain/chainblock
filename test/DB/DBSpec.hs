{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module DB.DBSpec (main, spec) where

import           Control.Monad.Except         (runExceptT)
import           Data.Either
import           Data.List                    (elem)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Database.PostgreSQL.Simple   (ConnectInfo (..), Connection,
                                               close, connect)
import           System.Environment           (getEnv)
import           Test.Hspec

import           ChainBlock.DB.Interface
import           ChainBlock.DB.Postgres
import           ChainBlock.DB.Postgres.Setup
import           ChainBlock.DB.Types
import           ChainBlock.Errors

main :: IO ()
main = hspec spec

spec :: Spec
spec = dbSpec

dbSpec :: Spec
dbSpec = do
    connInfo <- runIO buildConnectInfo
    conn <- runIO $ initDB connInfo
    dbi <- runIO $ databaseInterface conn runDBInterfaceIO
    afterAll_
      ( do
          close conn
          dropDB connInfo
      ) $
      describe "DB Spec" $ do
        it "should query all created users" $ do
          let usernames = ["user1", "user2", "user3", "user4"]
          eResInserts <- mapM (runExceptT . runDBI dbi . insertUser dbi . Username)
                              usernames
          mapM_ (shouldBe True . isRight)  eResInserts
          eResQuery <- runExceptT . runDBI dbi . queryAllUsers $ dbi
          isRight eResQuery `shouldBe` True
          let Right ress = eResQuery
          let inDB = map (\ (User {name=Username un}) -> elem un usernames)
                         ress
          mapM_ (shouldBe True) inDB
        it "should create a user and query the user" $ do
          eResInsert <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
          isRight eResInsert `shouldBe` True
          eResQuery <- runExceptT . runDBI dbi . queryUser dbi $ testUsername
          isRight eResQuery `shouldBe` True
          let Right resQuery = eResQuery
          name resQuery `shouldBe` testUsername
        it "should fail querying a non-existing user" $ do
          eResQuery <- runExceptT . runDBI dbi . queryUser dbi . Username $ "not-a-user"
          isLeft eResQuery `shouldBe` True
        it "should fail inserting existing user" $ do
          eResInsert <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
          isLeft eResInsert `shouldBe` True


------------------------------------------------------------------------------
--Spec Fixtures
------------------------------------------------------------------------------
testUsername :: Username
testUsername = Username "rob.machads"


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------

initDB :: ConnectInfo -> IO Connection
initDB connInfo = do
  let dbName = connectDatabase connInfo
  _ <- createDBIfNeeded connInfo dbName
  connect connInfo

