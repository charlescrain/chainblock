{-# LANGUAGE OverloadedStrings #-}

module APISpec (main, spec) where

import           Data.ByteString.Base16   (decode, encode)
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Network.Wreq             (Response, auth, basicAuth, defaults,
                                           getWith, post, postWith,
                                           responseBody)
import           Test.Hspec

import           ChainBlock.AppConfig     (AppConfig)
import           ChainBlock.Crypto

main :: IO ()
main = hspec spec

spec :: Spec
spec = apiSpec

apiSpec :: Spec
apiSpec =
    describe "API Spec return appropriate values" $
    around (testWithApplication app mkAppConfig) $
    it "getUsers" $ do
      pendingWith "Underconstruction"
      True `shouldBe` True

mkAppConfig :: AppConfig IO
mkAppConfig = undefined
