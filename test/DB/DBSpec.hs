{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module DB.DBSpec (main, spec) where

import           Control.Monad.Except         (ExceptT (..), runExceptT)
import           Data.Either
import           Data.List                    (elem)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Database.PostgreSQL.Simple   (ConnectInfo (..), Connection,
                                               close, connect)
import           System.Environment           (getEnv)
import           Test.Hspec
import           Test.QuickCheck.Arbitrary    (Arbitrary (..))
import           Test.QuickCheck.Gen          (generate)

import           ChainBlock.DB.Interface
import           ChainBlock.DB.Postgres
import           ChainBlock.DB.Postgres.Setup
import           ChainBlock.DB.Postgres.Types (PGDB (..))
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
      runDBSpecs dbi
  where
    runDBSpecs dbi = do
      userSpec dbi
      websiteSpec dbi
      credntialsSpec dbi

userSpec :: IDataBase PGDB (ExceptT CBError IO)  -> Spec
userSpec dbi =
  describe "User Spec" $ do
    it "should query all created users" $ do
      usernames <- mapM (\ _ -> generate arbitrary) [1..5]
      eResInserts <- mapM (runExceptT . runDBI dbi . insertUser dbi)
                          usernames
      mapM_ (shouldBe True . isRight)  eResInserts

      eResQuery <- runExceptT . runDBI dbi . queryAllUsers $ dbi
      isRight eResQuery `shouldBe` True

      let Right ress = eResQuery
          inDB = map (\ User {name=un} -> elem un usernames)
                     ress
      mapM_ (shouldBe True) inDB
    it "should create a user and query the user" $ do
      testUsername <- generate arbitrary
      eResInsert <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsert `shouldBe` True

      eResQuery <- runExceptT . runDBI dbi . queryUser dbi $ testUsername
      isRight eResQuery `shouldBe` True

      let Right resQuery = eResQuery
      name resQuery `shouldBe` testUsername
    it "should fail querying a non-existing user" $ do
      testUsername <- generate arbitrary
      eResQuery <- runExceptT . runDBI dbi . queryUser dbi $ testUsername
      isLeft eResQuery `shouldBe` True
    it "should fail inserting existing user" $ do
      testUsername <- generate arbitrary
      eResInsert <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsert `shouldBe` True

      eResInsertFail <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isLeft eResInsertFail `shouldBe` True
    it "should update a user with a new Username" $ do
      testUsername <- generate arbitrary
      eResInsert <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsert `shouldBe` True

      eResQuery <- runExceptT . runDBI dbi . queryUser dbi $ testUsername
      isRight eResQuery `shouldBe` True
      let Right user = eResQuery

      name user `shouldBe` testUsername
      testUsername' <- generate arbitrary
      eResUpdate <- runExceptT . runDBI dbi $ updateUser dbi (uId user) testUsername'
      isRight eResUpdate `shouldBe` True

      eResQuery' <- runExceptT . runDBI dbi . queryUser dbi $ testUsername'
      isRight eResQuery' `shouldBe` True
      let Right user' = eResQuery'
      name user' `shouldBe` testUsername'
    it "should fail updating non-existent user" $ do
      testUsername <- generate arbitrary
      nonExistentId <- generate arbitrary
      eResUpdate <- runExceptT . runDBI dbi $ updateUser dbi nonExistentId testUsername
      isRight eResUpdate `shouldBe` False
    it "should create and delete a user" $ do
      testUsername <- generate arbitrary
      eResInsert <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsert `shouldBe` True

      let Right uId' = eResInsert
      eResDelete <- runExceptT . runDBI dbi $ deleteUser dbi uId'
      isRight eResDelete `shouldBe` True
    it "should fail delete non-existent user" $ do
      nonExistentId <- generate arbitrary
      eResDelete <- runExceptT . runDBI dbi $ deleteUser dbi nonExistentId
      isRight eResDelete `shouldBe` False

websiteSpec :: IDataBase PGDB (ExceptT CBError IO)  -> Spec
websiteSpec dbi = describe "Website Spec" $ do
    it "should query all websites for a user" $ do
      pendingWith "Under consruction"
      let username' = Username "queryAllWebsites"
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ username'
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
          webdetails = [ (WebsiteURL "webURL1", WebsiteName "webname1")
                       , (WebsiteURL "webURL2", WebsiteName "webname2")
                       , (WebsiteURL "webURL3", WebsiteName "webname3")
                       , (WebsiteURL "webURL4", WebsiteName "webname4") ]
      eResInserts <- mapM (runExceptT . runDBI dbi . uncurry (insertWebsite dbi uId'))
                          webdetails
      mapM_ (shouldBe True . isRight)  eResInserts

      eResQuery <- runExceptT . runDBI dbi $ queryWebsites dbi uId'
      isRight eResQuery `shouldBe` True

      let Right ress = eResQuery
          inDB = map (\ w -> (websiteURL w, websiteName w) `elem` webdetails)
                     ress
      mapM_ (shouldBe True) inDB
    it "should create a website and query the website" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
          weburl = WebsiteURL "http://oneringtorule.com"
          webname = WebsiteName "OneRingtoRule"
      eResInsert <- runExceptT . runDBI dbi $ insertWebsite dbi uId' weburl webname
      isRight eResInsert `shouldBe` True

      let Right webId' = eResInsert
      eResQuery <- runExceptT . runDBI dbi . queryWebsite dbi $ webId'
      isRight eResQuery `shouldBe` True
      let Right website = eResQuery
      websiteName website `shouldBe` webname
    it "should fail querying a non-existing website" $ do
      eResQuery <- runExceptT . runDBI dbi . queryWebsite dbi . WebsiteId $ 1999
      isLeft eResQuery `shouldBe` True
    it "should fail inserting existing website" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      eResInsert <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isLeft eResInsert `shouldBe` True
    it "should update a website with a new details" $
      pendingWith "Un-implemented"
    it "should fail updating non-existent website" $
      pendingWith "Un-implemented"
    it "should delete a website" $
      pendingWith "Un-implemented"
    it "should fail delete non-existent website" $
      pendingWith "Un-implemented"

credntialsSpec :: IDataBase PGDB (ExceptT CBError IO)  -> Spec
credntialsSpec _dbi = describe "Credentials Spec" $ do
    it "should create a credentials and query the credentials" $
      pendingWith "Un-implemented"
    it "should fail querying a non-existing credentials" $
      pendingWith "Un-implemented"
    it "should fail inserting existing credentials" $
      pendingWith "Un-implemented"
    it "should update a credentials with a new details" $
      pendingWith "Un-implemented"
    it "should fail updating non-existent credentials" $
      pendingWith "Un-implemented"
    it "should delete a credentials" $
      pendingWith "Un-implemented"
    it "should fail delete non-existent credentials" $
      pendingWith "Un-implemented"


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------

initDB :: ConnectInfo -> IO Connection
initDB connInfo = do
  let dbName = connectDatabase connInfo
  _ <- createDBIfNeeded connInfo dbName
  connect connInfo

