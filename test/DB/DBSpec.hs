{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.DBSpec (main, spec) where

import           Control.Monad.Except       (ExceptT (..), runExceptT)
import           Data.Either
import           Data.List                  (elem)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             close, connect)
import           Test.Hspec
import           Test.QuickCheck.Arbitrary  (Arbitrary (..))
import           Test.QuickCheck.Gen        (generate)

import           Tholos.DB.Interface
import           Tholos.DB.Postgres
import           Tholos.DB.Postgres.Setup
import           Tholos.Types
import           Tholos.Errors

main :: IO ()
main = hspec spec

spec :: Spec
spec = dbSpec


dbSpec :: Spec
dbSpec =
  describe "Data layer Tests" $ do
    connInfo <- runIO buildConnectInfo
    conn <- runIO $ initDB connInfo
    dbi <- runIO $ createInterface conn runInterfaceExceptT
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
      credentialsSpec dbi

userSpec :: IDataBase (ExceptT CBError IO)  -> Spec
userSpec dbi =
  describe "User Spec" $ do
    it "should query all created users" $ do
      usernames <- mapM (\ _ -> generate arbitrary) [0..4 :: Int]
      eResInserts <- mapM (runExceptT . insertUser dbi)
                          usernames
      mapM_ (shouldBe True . isRight)  eResInserts

      eResQuery <- runExceptT . queryAllUsers $ dbi
      isRight eResQuery `shouldBe` True

      let Right ress = eResQuery
          inDB = map (\ User {name=un} -> elem un usernames)
                     ress
      mapM_ (shouldBe True) inDB
    it "should create a user and query the user" $ do
      testUsername <- generate arbitrary
      eResInsert <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsert `shouldBe` True

      eResQuery <- runExceptT . queryUser dbi $ testUsername
      isRight eResQuery `shouldBe` True

      let Right resQuery = eResQuery
      name resQuery `shouldBe` testUsername
    it "should fail querying a non-existing user" $ do
      testUsername <- generate arbitrary
      eResQuery <- runExceptT . queryUser dbi $ testUsername
      isLeft eResQuery `shouldBe` True
    it "should fail inserting existing user" $ do
      testUsername <- generate arbitrary
      eResInsert <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsert `shouldBe` True

      eResInsertFail <- runExceptT . insertUser dbi $ testUsername
      isLeft eResInsertFail `shouldBe` True
    it "should update a user with a new Username" $ do
      testUsername <- generate arbitrary
      eResInsert <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsert `shouldBe` True

      let Right uId' = eResInsert
      testUsername' <- generate arbitrary
      eResUpdate <- runExceptT $ updateUser dbi uId' testUsername'
      isRight eResUpdate `shouldBe` True

      eResQuery' <- runExceptT $ queryUser dbi testUsername'
      isRight eResQuery' `shouldBe` True
      let Right user' = eResQuery'
      name user' `shouldBe` testUsername'
    it "should fail updating non-existent user" $ do
      testUsername <- generate arbitrary
      nonExistentId <- generate arbitrary
      eResUpdate <- runExceptT $ updateUser dbi nonExistentId testUsername
      isLeft eResUpdate `shouldBe` True
    it "should create and delete a user" $ do
      testUsername <- generate arbitrary
      eResInsert <- runExceptT $ insertUser dbi testUsername
      isRight eResInsert `shouldBe` True

      let Right uId' = eResInsert
      eResDelete <- runExceptT $ deleteUser dbi uId'
      isRight eResDelete `shouldBe` True
    it "should fail delete non-existent user" $ do
      nonExistentId <- generate arbitrary
      eResDelete <- runExceptT $ deleteUser dbi nonExistentId
      isLeft eResDelete `shouldBe` True

websiteSpec :: IDataBase (ExceptT CBError IO)  -> Spec
websiteSpec dbi = describe "Website Spec" $ do
    it "should query all websites for a user" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
          siteIndexes = [0..4]
      testWebURLs :: [WebsiteURL] <- mapM (const (generate arbitrary)) siteIndexes
      testWebNames :: [WebsiteName] <- mapM (const (generate arbitrary)) siteIndexes
      eResInserts <- mapM (\i ->
                              let webURL = testWebURLs !! i
                                  webName = testWebNames !! i
                              in runExceptT $ insertWebsite dbi uId' webURL webName )
                           siteIndexes
      mapM_ (shouldBe True . isRight)  eResInserts

      eResQuery <- runExceptT $ queryWebsites dbi uId'
      isRight eResQuery `shouldBe` True
      let Right ress = eResQuery
          inDB = map (\ w -> websiteURL w `elem` testWebURLs && websiteName w `elem` testWebNames)
                     ress
      mapM_ (shouldBe True) inDB
    it "should create a website and query the website" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsert <- runExceptT $ insertWebsite dbi uId' weburl webname
      isRight eResInsert `shouldBe` True

      let Right webId' = eResInsert
      eResQuery <- runExceptT . queryWebsite dbi $ webId'
      isRight eResQuery `shouldBe` True
      let Right website = eResQuery
      websiteName website `shouldBe` webname
      websiteURL website `shouldBe` weburl

    it "should fail querying a non-existing website" $ do
      webId' <- generate arbitrary
      eResQuery <- runExceptT . queryWebsite dbi $ webId'
      isLeft eResQuery `shouldBe` True

    it "should update a website with a new details" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsert <- runExceptT $ insertWebsite dbi uId' weburl webname
      isRight eResInsert `shouldBe` True

      let Right webId' = eResInsert
      weburl' <- generate arbitrary
      webname' <- generate arbitrary
      eResUpdate <- runExceptT $ updateWebsite dbi webId' weburl' webname'
      isRight eResUpdate `shouldBe` True

    it "should fail updating non-existent website" $ do
      webId' <- generate arbitrary
      weburl' <- generate arbitrary
      webname' <- generate arbitrary
      eResUpdate <- runExceptT $ updateWebsite dbi webId' weburl' webname'
      isRight eResUpdate `shouldBe` False

    it "should delete a website" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsert <- runExceptT $ insertWebsite dbi uId' weburl webname
      isRight eResInsert `shouldBe` True

      let Right webId' = eResInsert
      eResDelete <- runExceptT $ deleteWebsite dbi webId'
      isRight eResDelete `shouldBe` True

    it "should fail delete non-existent website" $ do
      webId' <- generate arbitrary
      eResDelete <- runExceptT $ deleteWebsite dbi webId'
      isRight eResDelete `shouldBe` False

credentialsSpec :: IDataBase (ExceptT CBError IO)  -> Spec
credentialsSpec dbi = describe "Credentials Spec" $ do
    it "should create a credentials and query the credentials" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsertWeb <- runExceptT $ insertWebsite dbi uId' weburl webname
      isRight eResInsertWeb `shouldBe` True

      let Right webId' = eResInsertWeb
          credIndexes = [0..4]
      encryptedPasswords  :: [EncryptedPassword] <- mapM (const (generate arbitrary)) credIndexes
      webUserNames :: [WebUsername] <- mapM (const (generate arbitrary)) credIndexes
      eResInserts <- mapM (\i ->
                              let webPass = encryptedPasswords !! i
                                  webUName = webUserNames !! i
                              in runExceptT $ insertCredentials dbi uId' webId' webPass webUName)
                           credIndexes
      mapM_ (shouldBe True . isRight)  eResInserts

      eResQuery <- runExceptT $ queryAllUserCredentials dbi uId'
      mapM_ (shouldBe True . isRight)  eResInserts
      let Right ress = eResQuery
          inDB = map (\ c -> credUserId c == uId'
                          && webId c == webId'
                          && webUsername c `elem` webUserNames
                          && encPassword c `elem` encryptedPasswords)
                     ress
      mapM_ (shouldBe True) inDB

    it "should create and query credentials" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsertWeb <- runExceptT $ insertWebsite dbi uId' weburl webname
      isRight eResInsertWeb `shouldBe` True

      let Right webId' = eResInsertWeb
      encryptedPassword <- generate arbitrary
      webUserName <- generate arbitrary
      eResInsert <- runExceptT $ insertCredentials dbi uId' webId' encryptedPassword webUserName
      isRight eResInsert `shouldBe` True

      let Right cId' = eResInsert
      eResQuery <- runExceptT $ queryCredentials dbi cId'
      isRight eResQuery `shouldBe` True
      let Right cred = eResQuery
      webUsername cred `shouldBe` webUserName
      encPassword cred `shouldBe` encryptedPassword

    it "should fail querying a non-existing credentials" $ do
      credId' <- generate arbitrary
      eResQuery <- runExceptT . queryCredentials dbi $ credId'
      isLeft eResQuery `shouldBe` True

    it "should update a credentials with a new details" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsertWeb <- runExceptT $ insertWebsite dbi uId' weburl webname
      isRight eResInsertWeb `shouldBe` True

      let Right webId' = eResInsertWeb
      encryptedPassword <- generate arbitrary
      webUserName <- generate arbitrary
      eResInsert <- runExceptT $ insertCredentials dbi uId' webId' encryptedPassword webUserName
      isRight eResInsert `shouldBe` True

      let Right cId' = eResInsert
      webUserName' <- generate arbitrary
      encryptedPassword' <- generate arbitrary
      eResUpdate <- runExceptT $ updateCredentials dbi cId' encryptedPassword' webUserName'
      isRight eResUpdate `shouldBe` True

    it "should fail updating non-existent credentials" $ do
      cId' <- generate arbitrary
      pass' <- generate arbitrary
      webname' <- generate arbitrary
      eResUpdate <- runExceptT $ updateWebsite dbi cId' pass' webname'
      isLeft eResUpdate `shouldBe` True

    it "should delete a credentials" $ do
      testUsername <- generate arbitrary
      eResInsertUser <- runExceptT . insertUser dbi $ testUsername
      isRight eResInsertUser `shouldBe` True

      let Right uId' = eResInsertUser
      weburl <- generate arbitrary
      webname <- generate arbitrary
      eResInsertWeb <- runExceptT $ insertWebsite dbi uId' weburl webname
      isRight eResInsertWeb `shouldBe` True

      let Right webId' = eResInsertWeb
      encryptedPassword <- generate arbitrary
      webUserName <- generate arbitrary
      eResInsert <- runExceptT $ insertCredentials dbi uId' webId' encryptedPassword webUserName
      isRight eResInsert `shouldBe` True

      let Right cId' = eResInsert
      eResDelete <- runExceptT $ deleteCredentials dbi cId'
      isRight eResDelete `shouldBe` True

    it "should fail deleting non-existent credentials" $ do
      cId' <- generate arbitrary
      eResDelete <- runExceptT $ deleteCredentials dbi cId'
      isLeft eResDelete `shouldBe` True


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------

initDB :: ConnectInfo -> IO Connection
initDB connInfo = do
  let dbName = connectDatabase connInfo
  _ <- createDBIfNeeded connInfo dbName
  connect connInfo

