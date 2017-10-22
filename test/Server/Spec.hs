{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Spec (main, spec) where

-- import           Control.Lens.Lens         ((&))
import           Data.Aeson
-- import           Data.ByteString.Base16    (decode, encode)
import qualified Data.ByteString.Lazy         as BSL
import           Data.Monoid                  ((<>))
-- import           Data.Text                 (Text)
import           Data.Either
import           Network.Wai.Handler.Warp     (testWithApplication)
import           Network.Wreq                 (Response, defaults, getWith,
                                               postWith, responseBody)
import           Network.Wreq.Types           (Options (..))
import           Test.Hspec
import           Test.QuickCheck.Arbitrary    (Arbitrary (..))
import           Test.QuickCheck.Gen          (generate)

import           Tholos.API.Types
import           Tholos.API.Class
import           Tholos.Server.V0
import           Tholos.Types
import           Tholos.App
import           Tholos.AppConfig.Environment
-- import           Tholos.Crypto

main :: IO ()
main = hspec spec

spec :: Spec
spec = serverSpec

serverSpec :: Spec
serverSpec =
    describe "Server Spec" $ do
      it "should post a new user and get a userId" $ do
        un <- generate arbitrary
        res <- postUserEntryPoint un
        expected <- insertUser un
        res `shouldBe` expected
      it "should get a list of users" $ do
        res <- getUsersEntryPoint
        expected <- getUsers
        res `shouldBe` expected
      it "should post a new website" $ do
        userid <- generate arbitrary
        weburl <- generate arbitrary
        webname <- generate arbitrary
        res <- postWebsiteEntryPoint userid (PostWebsite weburl webname)
        expected <- undefined
        res `shouldBe` expected


------------------------------------------------------------------------------
-- | Spec Utils
------------------------------------------------------------------------------

instance DBModifyUser IO where
  insertUser _ = return $ UserId 1

instance DBQueryUser IO where
  getUsers = return $ [ User (Username "taj-burrow") (UserId 1)
                      , User (Username "kelly-slates") (UserId 2)
                      ]
             
instance DBModifyWebsite IO where
  insertWebsite _ _ _ = return $ WebsiteId 1
-- mkAppConfig :: IO AppConfig
-- mkAppConfig = return $ AppConfig Test 8000
-- 
-- runTestT :: TestT a -> Either TholosError a
-- runTestT =undefined
-- 
-- newtype TestT = TestT {unTestT :: }
