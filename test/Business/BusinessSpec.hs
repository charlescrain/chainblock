{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Business.BusinessSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck.Arbitrary  (Arbitrary (..))
import           Data.Either
import           Test.QuickCheck.Gen        (generate, listOf)

import           Tholos.Business
import           Tholos.Business.Interface
import           Tholos.Business.Types
import           Tholos.DB.Interface
import           Tholos.Errors
import           Tholos.Monad

main :: IO ()
main = hspec spec

spec :: Spec
spec = bzSpec


bzSpec :: Spec
bzSpec =
  describe "Business layer Tests" $ do
    dbI <- runIO generateDBI
    runBusniessSpecs dbI
  where
    runBusniessSpecs dbI = do
      userSpec dbI
      websiteSpec dbI
   -- credntialsSpec dbi

userSpec :: IDataBase CommonT -> Spec
userSpec dbi  =
  describe "User Spec" $ do
    it "should get a list of users" $ do
      bzi <- createInterface (runInterfaceCommonT dbi)
      Right testusers <- runCommonT $ queryAllUsers dbi
      eResGet <- runCommonT . getUsers $ bzi
      isRight eResGet `shouldBe` True
      let Right users = eResGet
      users `shouldBe` testusers
    it "should post a user and get an id" $ do
      bzi <- createInterface (runInterfaceCommonT dbi)
      username <- generate arbitrary
      postUserBody <- generate arbitrary
      Right dbUserId <- runCommonT $ insertUser dbi username 
      eResPost <- runCommonT $ postUser bzi postUserBody
      isRight eResPost `shouldBe` True
      let Right userId = eResPost
      userId `shouldBe` dbUserId
    it "should post a user and handle duplciate key error" $
      pendingWith "unimplemented"

websiteSpec :: IDataBase CommonT -> Spec
websiteSpec bzI = describe "Website Spec" $ do
    it "should get a list of websites" $
      pendingWith "unimplemented"
    it "should post a website and get an id" $
      pendingWith "unimplemented"


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------

generateDBI :: IO (IDataBase CommonT)
generateDBI = do
  queryAllUsers' <- generate (listOf arbitrary)
  queryUser'     <- generate arbitrary
  insertUser'    <- generate arbitrary

  queryWebsites' <- generate (listOf arbitrary)
  queryWebsite'  <- generate arbitrary
  insertWebsite' <- generate arbitrary

  queryAllUserCredentials' <- generate (listOf arbitrary)
  queryCredentials'        <- generate arbitrary
  insertCredentials'       <- generate arbitrary
  return IDataBase { -- User
                     queryAllUsers = return queryAllUsers'
                   , queryUser     = (\_ -> return queryUser')
                   , insertUser    = (\_ -> return insertUser')
                   , updateUser    = (\_ _ -> return ())
                   , deleteUser    = return . const ()

                     -- Website
                   , queryWebsites = (\_ -> return queryWebsites')
                   , queryWebsite  = (\_ -> return queryWebsite')
                   , insertWebsite = (\_ _ _ -> return insertWebsite')
                   , updateWebsite = (\_ _ _ -> return ())
                   , deleteWebsite = return . const ()

                     -- Credentials
                   , queryAllUserCredentials = (\_ -> return queryAllUserCredentials')
                   , queryCredentials  = (\_ -> return queryCredentials')
                   , insertCredentials = (\_ _ _ _ -> return insertCredentials')
                   , updateCredentials = (\_ _ _ -> return ())
                   , deleteCredentials = return . const ()
                   }

