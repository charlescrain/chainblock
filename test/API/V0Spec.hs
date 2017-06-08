{-# LANGUAGE OverloadedStrings #-}

module API.V0Spec (main, spec) where

import           Data.ByteString.Base16   (decode, encode)
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Network.Wreq             (Response, auth, basicAuth, defaults,
                                           getWith, post, postWith,
                                           responseBody)
import           Test.Hspec

import           App
import           ChainBlock.Crypto
import           Main

main :: IO ()
main = hspec spec

spec :: Spec
spec = apiSpec

routePrefix = "/v0"

apiSpec :: Spec
apiSpec =
    describe "API Spec return appropriate values" $
    around (testWithApplication (app mkAppConfig)) $ do
      it "should call GET /users and return a list of existing users" $ \port -> do
        pendingWith "Underconstruction"
        True `shouldBe` True
      it "should call POST /users and return a new userId" $ \port -> do
        pendingWith "Underconstruction"
        True `shouldBe` True
      it "should call GET /users/:userId/websites and return a list of website details" $ \port -> do
        pendingWith "Underconstruction"
        True `shouldBe` True
      it "should call POST /users/:userId/websites and return a new websiteId" $ do
        pendingWith "Underconstruction"
        True `shouldBe` True
      it "should call POST /users/:userId/websites/get/:websiteId and return a new websiteId" $ do
        pendingWith "Underconstruction"
        True `shouldBe` True

mkAppConfig :: AppConfig IO
mkAppConfig = undefined
