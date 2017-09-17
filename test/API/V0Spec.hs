{-# LANGUAGE OverloadedStrings #-}

module API.V0Spec (main, spec) where

import           Control.Lens.Lens             ((&))
import           Data.Aeson                    (toJSON)
import           Data.ByteString.Base16        (decode, encode)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import           Network.Wai.Handler.Warp      (testWithApplication)
import           Network.Wreq                  (Response, defaults, getWith,
                                                postWith, responseBody)
import           Network.Wreq.Types            (Options (..))
import           Test.Hspec

import           App
import           ChainBlock.API.Types
import           ChainBlock.Business.Interface
import           ChainBlock.Crypto
import           Config.Environment

main :: IO ()
main = hspec spec

spec :: Spec
spec = apiSpec

apiSpec :: Spec
apiSpec =
    describe "API Spec return appropriate values" $
    around (testWithApplication (app <$> mkAppConfig)) $ do
      it "should call GET /users and return a list of existing users" $ \port -> do
        pendingWith "Underconstruction"
        resp <- get "/users" port
        True `shouldBe` True
      it "should call POST /users and return a new userId" $ \port -> do
        pendingWith "Underconstruction"
        let user = toJSON . PostUserBody $ (Username "Sabriel")
        resp <- post "/users" port user
        True `shouldBe` True
      it "should call GET /users/:userId/websites and return a list of website details" $ \port -> do
        pendingWith "Underconstruction"
        let user = toJSON . PostUserBody $ (Username "Sabriel")
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

options = defaults {headers=[("Accept","application/vnd.api+json")]}

buildUrl route port = "http://localhost:" <> (show port) <> routePrefix <> route

get r p = getWith options (buildUrl r p)

post r p = postWith options (buildUrl r p)

mkAppConfig :: IO (AppConfig IO IO)
mkAppConfig = return $ AppConfig Test 8000 irf

irf :: IBusinessFunctions IO IO
irf = undefined
