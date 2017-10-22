{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.Spec (main, spec) where

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
import           Tholos.Types
import           Tholos.App
import           Tholos.AppConfig.Environment
import           Tholos.Business.Interface
-- import           Tholos.Crypto

main :: IO ()
main = hspec spec

spec :: Spec
spec = apiSpec

apiSpec :: Spec
apiSpec =
    describe "API Spec" $
    around (testWithApplication (app <$> mkAppConfig)) $ do
      it "should call POST /users and return a new userId" $ \port -> do
        user <- toJSON <$> (generate arbitrary :: IO PostUserBody)
        eResp :: Either String UserId <- decodeResponse <$> post "/users" port user
        isRight eResp `shouldBe` True
      it "should call GET /users and return a list of existing users" $ \port -> do
        pendingWith "Underconstruction"
        resp <- get "/users" port
        True `shouldBe` True
      it "should call GET /users/:userId/websites and return a list of website details" $ \port -> do
        pendingWith "Underconstruction"
        user <- toJSON <$> (generate arbitrary :: IO PostUserBody)
        resp <- get "/users" port
        True `shouldBe` True
      it "should call POST /users/:userId/websites and return a new websiteId" $ \port -> do
        pendingWith "Underconstruction"
        True `shouldBe` True
      it "should call POST /users/:userId/websites/get/:websiteId and return a new websiteId" $ \port -> do
        pendingWith "Underconstruction"
        True `shouldBe` True

routePrefix = "/api/v0"

------------------------------------------------------------------------------
--Spec Fixtures
------------------------------------------------------------------------------


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------

decodeResponse :: FromJSON m => Response BSL.ByteString -> Either String m
decodeResponse = undefined

options = defaults --{headers=[("Content-Type","application/json")]}

buildUrl route port = "http://localhost:" <> (show port) <> routePrefix <> route

get r p = getWith options (buildUrl r p)

post r p = postWith options (buildUrl r p)

mkAppConfig :: IO AppConfig
mkAppConfig = return $ AppConfig Test 8000

