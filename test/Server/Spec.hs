{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Spec (main, spec) where

-- import           Control.Lens.Lens         ((&))
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
import           Tholos.App.Environment
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
      it "should post a website and get a websiteId" $ do
        userid <- generate arbitrary
        weburl <- generate arbitrary
        webname <- generate arbitrary
        res <- postWebsiteEntryPoint userid (PostWebsite weburl webname)
        expected <- insertWebsite userid weburl webname
        res `shouldBe` expected
      it "should get a websites for a user" $ do
        userid <- generate arbitrary
        res <- getWebsitesEntryPoint userid 
        expected <- getWebsites userid
        res `shouldBe` expected
      it "should post website credentials for a user" $ do
        postcreds <- generate arbitrary
        userid <- generate arbitrary
        webid <- generate arbitrary
        res <- postCredentialsEntryPoint userid webid postcreds
        let expected = ()
        res `shouldBe` expected
      it "should get a website credentials for a user" $ do
        pmk@(PostMasterKey tpmk) <- generate arbitrary
        userid <- generate arbitrary
        webid <- generate arbitrary
        ep <- generate arbitrary
        res <- getCredentialsEntryPoint userid webid pmk
        expectPass <- decrypt tpmk ep
        expectusername <- (webUsername . head) <$> getCredentials userid webid
        expectWeb <- getWebsite userid webid
        let expectWebCreds = [ WebsiteCredentials { username = expectusername
                                                  , password = expectPass
                                                  } ]
            expected = Website { websiteDetails = expectWeb
                               , websiteCredentials = expectWebCreds
                               }
        res `shouldBe` expected


------------------------------------------------------------------------------
-- | Spec Instances
------------------------------------------------------------------------------

instance DBModifyUser IO where
  insertUser _ = return $ UserId 1

instance DBQueryUser IO where
  getUsers = return $ [ User (Username "taj-burrow") (UserId 1)
                      , User (Username "kelly-slates") (UserId 2)
                      ]

instance DBModifyWebsite IO where
  insertWebsite _ _ _ = return $ WebsiteId 1

instance DBQueryWebsite IO where
  getWebsites uId = do
    let wDetails = WebsiteDetails { websiteURL = WebsiteURL "http://barrels.com"
                                  , websiteName = WebsiteName "Barrels"
                                  , websiteId = WebsiteId 1
                                  , userId = uId
                                  }
    return  [wDetails]

  getWebsite uId wid = do
    let wDetails = WebsiteDetails { websiteURL = WebsiteURL "http://barrels.com"
                                  , websiteName = WebsiteName "Barrels"
                                  , websiteId = wid
                                  , userId = uId
                                  }
    return  wDetails

instance Crypto IO where
  encrypt _ _ = return $ EncryptedPassword "ERMAGAAWWDNOTBARRELS"
  decrypt _ _ = return $ PlainTextPassword "BARRELS"

instance DBModifyCredentials IO where
  insertCredentials _ _ _ _ = return $ CredentialsId 1

instance DBQueryCredentials IO where
  getCredentials uid wid  = do
    let creds = Credentials { credId = CredentialsId 1
                            , webUsername = WebUsername "gandolfthegrayest"
                            , encPassword = EncryptedPassword "ERMAGAAWDNOTBARRELS"
                            , webId = wid
                            , credUserId = uid
                            }
    return [creds]

-- mkAppConfig :: IO AppConfig
-- mkAppConfig = return $ AppConfig Test 8000
-- 
-- runTestT :: TestT a -> Either TholosError a
-- runTestT =undefined
-- 
-- newtype TestT = TestT {unTestT :: }
