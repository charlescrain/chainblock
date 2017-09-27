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
import           Tholos.Types
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
userSpec dbI  =
  describe "User Spec" $ do
    it "should get a list of users" $ do
      bzI <- createInterface (runInterfaceCommonT dbI)
      Right usersFromDB <- runCommonT $ queryAllUsers dbI

      eResGet <- runCommonT . getUsers $ bzI
      isRight eResGet `shouldBe` True
      let Right users = eResGet
      users `shouldBe` usersFromDB
    it "should post a user and get an id" $ do
      bzI <- createInterface (runInterfaceCommonT dbI)
      username <- generate arbitrary
      postUserBody <- generate arbitrary
      Right userIdFromDB <- runCommonT $ insertUser dbI username

      eResPost <- runCommonT $ postUser bzI postUserBody
      isRight eResPost `shouldBe` True
      let Right userId = eResPost
      userId `shouldBe` userIdFromDB

websiteSpec :: IDataBase CommonT -> Spec
websiteSpec dbI = describe "Website Spec" $ do
    it "should get a list of websites" $ do
      bzI <- createInterface (runInterfaceCommonT dbI)
      userid <- generate arbitrary
      Right websitesFromDB <- runCommonT $ queryWebsites dbI userid

      eResGet <- runCommonT $ getWebsites bzI  userid
      isRight eResGet `shouldBe` True
      let Right websites  = eResGet
      websites `shouldBe` websitesFromDB
    it "should post a website and get an id" $ do
      bzI <- createInterface (runInterfaceCommonT dbI)
      userid <- generate arbitrary
      wurl <- generate arbitrary
      wname <- generate arbitrary
      let postWeb = PostWebsite wurl wname
      Right webIdInDB <- runCommonT $ insertWebsite dbI userid wurl wname

      eResPost <- runCommonT $ postWebsite bzI userid postWeb
      isRight eResPost `shouldBe` True
      let Right wid  = eResPost
      wid `shouldBe` webIdInDB


------------------------------------------------------------------------------
-- | Spec Utils
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

