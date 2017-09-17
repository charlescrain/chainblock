{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
      usernames <- mapM (\ _ -> generate arbitrary) [0..4]
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

      let Right uId' = eResInsert
      testUsername' <- generate arbitrary
      eResUpdate <- runExceptT . runDBI dbi $ updateUser dbi uId' testUsername'
      isRight eResUpdate `shouldBe` True

      eResQuery' <- runExceptT . runDBI dbi . queryUser dbi $ testUsername'
      isRight eResQuery' `shouldBe` True
      let Right user' = eResQuery'
      name user' `shouldBe` testUsername'
    it "should fail updating non-existent user" $ do
      testUsername <- generate arbitrary
      nonExistentId <- generate arbitrary
      eResUpdate <- runExceptT . runDBI dbi $ updateUser dbi nonExistentId testUsername
      isLeft eResUpdate `shouldBe` True
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
      isLeft eResDelete `shouldBe` True

websiteSpec :: IDataBase PGDB (ExceptT CBError IO)  -> Spec
websiteSpec dbi = describe "Website Spec" $ do
    it "should query all websites for a user" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
          siteIndexes = [0..4]
      testWebURLs :: [WebsiteURL] <- mapM (const (generate arbitrary)) siteIndexes
      testWebNames :: [WebsiteName] <- mapM (const (generate arbitrary)) siteIndexes
      eResInserts <- mapM (\i ->
                              let webURL = testWebURLs !! i
                                  webName = testWebNames !! i
                              in runExceptT . runDBI dbi $ insertWebsite dbi uId' webURL webName )
                           siteIndexes
      mapM_ (shouldBe True . isRight)  eResInserts

      eResQuery <- runExceptT . runDBI dbi $ queryWebsites dbi uId'
      isRight eResQuery `shouldBe` True
      let Right ress = eResQuery
          inDB = map (\ w -> websiteURL w `elem` testWebURLs && websiteName w `elem` testWebNames)
                     ress
      mapM_ (shouldBe True) inDB
    it "should create a website and query the website" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsert <- runExceptT . runDBI dbi $ insertWebsite dbi uId' weburl webname
      isRight eResInsert `shouldBe` True

      let Right webId' = eResInsert
      eResQuery <- runExceptT . runDBI dbi . queryWebsite dbi $ webId'
      isRight eResQuery `shouldBe` True
      let Right website = eResQuery
      websiteName website `shouldBe` webname
      websiteURL website `shouldBe` weburl

    it "should fail querying a non-existing website" $ do
      webId' <- generate arbitrary
      eResQuery <- runExceptT . runDBI dbi . queryWebsite dbi $ webId'
      isLeft eResQuery `shouldBe` True

    it "should update a website with a new details" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsert <- runExceptT . runDBI dbi $ insertWebsite dbi uId' weburl webname
      isRight eResInsert `shouldBe` True

      let Right webId' = eResInsert
      weburl' <- generate arbitrary
      webname' <- generate arbitrary
      eResUpdate <- runExceptT . runDBI dbi $ updateWebsite dbi webId' weburl' webname'
      isRight eResUpdate `shouldBe` True

    it "should fail updating non-existent website" $ do
      webId' <- generate arbitrary
      weburl' <- generate arbitrary
      webname' <- generate arbitrary
      eResUpdate <- runExceptT . runDBI dbi $ updateWebsite dbi webId' weburl' webname'
      isRight eResUpdate `shouldBe` False

    it "should delete a website" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsert <- runExceptT . runDBI dbi $ insertWebsite dbi uId' weburl webname
      isRight eResInsert `shouldBe` True

      let Right webId' = eResInsert
      eResDelete <- runExceptT . runDBI dbi $ deleteWebsite dbi webId'
      isRight eResDelete `shouldBe` True

    it "should fail delete non-existent website" $ do
      webId' <- generate arbitrary
      eResDelete <- runExceptT . runDBI dbi $ deleteWebsite dbi webId'
      isRight eResDelete `shouldBe` False

credntialsSpec :: IDataBase PGDB (ExceptT CBError IO)  -> Spec
credntialsSpec dbi = describe "Credentials Spec" $ do
    it "should create a credentials and query the credentials" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsertWeb <- runExceptT . runDBI dbi $ insertWebsite dbi uId' weburl webname
      isRight eResInsertWeb `shouldBe` True

      let Right webId' = eResInsertWeb
          credIndexes = [0..4]
      encryptedPasswords  :: [EncryptedPassword] <- mapM (const (generate arbitrary)) credIndexes
      webUserNames :: [WebUsername] <- mapM (const (generate arbitrary)) credIndexes
      eResInserts <- mapM (\i ->
                              let webPass = encryptedPasswords !! i
                                  webUName = webUserNames !! i
                              in runExceptT . runDBI dbi $ insertCredentials dbi uId' webId' webPass webUName)
                           credIndexes
      mapM_ (shouldBe True . isRight)  eResInserts

      eResQuery <- runExceptT . runDBI dbi $ queryAllUserCredentials dbi uId'
      mapM_ (shouldBe True . isRight)  eResInserts
      let Right ress = eResQuery
          inDB = map (\ c -> credUserId c == uId'
                          && webId c == webId'
                          && username c `elem` webUserNames
                          && password c `elem` encryptedPasswords)
                     ress
      mapM_ (shouldBe True) inDB

    it "should create and query credentials" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsertWeb <- runExceptT . runDBI dbi $ insertWebsite dbi uId' weburl webname
      isRight eResInsertWeb `shouldBe` True

      let Right webId' = eResInsertWeb
      encryptedPassword <- generate arbitrary
      webUserName <- generate arbitrary
      eResInsert <- runExceptT . runDBI dbi $ insertCredentials dbi uId' webId' encryptedPassword webUserName
      isRight eResInsert `shouldBe` True

      let Right cId' = eResInsert
      eResQuery <- runExceptT . runDBI dbi $ queryCredentials dbi cId'
      isRight eResQuery `shouldBe` True
      let Right cred = eResQuery
      username cred `shouldBe` webUserName
      password cred `shouldBe` encryptedPassword

    it "should fail querying a non-existing credentials" $ do
      credId' <- generate arbitrary
      eResQuery <- runExceptT . runDBI dbi . queryCredentials dbi $ credId'
      isLeft eResQuery `shouldBe` True

    it "should update a credentials with a new details" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsertWeb <- runExceptT . runDBI dbi $ insertWebsite dbi uId' weburl webname
      isRight eResInsertWeb `shouldBe` True

      let Right webId' = eResInsertWeb
      encryptedPassword <- generate arbitrary
      webUserName <- generate arbitrary
      eResInsert <- runExceptT . runDBI dbi $ insertCredentials dbi uId' webId' encryptedPassword webUserName
      isRight eResInsert `shouldBe` True

      let Right cId' = eResInsert
      webUserName' <- generate arbitrary
      encryptedPassword' <- generate arbitrary
      eResUpdate <- runExceptT . runDBI dbi $ updateCredentials dbi cId' encryptedPassword' webUserName'
      isRight eResUpdate `shouldBe` True

    it "should fail updating non-existent credentials" $ do
      cId' <- generate arbitrary
      pass' <- generate arbitrary
      webname' <- generate arbitrary
      eResUpdate <- runExceptT . runDBI dbi $ updateWebsite dbi cId' pass' webname'
      isLeft eResUpdate `shouldBe` True

    it "should delete a credentials" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . runDBI dbi . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsertWeb <- runExceptT . runDBI dbi $ insertWebsite dbi uId' weburl webname
      isRight eResInsertWeb `shouldBe` True

      let Right webId' = eResInsertWeb
      encryptedPassword <- generate arbitrary
      webUserName <- generate arbitrary
      eResInsert <- runExceptT . runDBI dbi $ insertCredentials dbi uId' webId' encryptedPassword webUserName
      isRight eResInsert `shouldBe` True

      let Right cId' = eResInsert
      eResDelete <- runExceptT . runDBI dbi $ deleteCredentials dbi cId'
      isRight eResDelete `shouldBe` True

    it "should fail deleting non-existent credentials" $ do
      cId' <- generate arbitrary
      eResDelete <- runExceptT . runDBI dbi $ deleteCredentials dbi cId'
      isLeft eResDelete `shouldBe` True


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------

initDB :: ConnectInfo -> IO Connection
initDB connInfo = do
  let dbName = connectDatabase connInfo
  _ <- createDBIfNeeded connInfo dbName
  connect connInfo

