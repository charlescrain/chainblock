{-# LANGUAGE OverloadedStrings #-}

module PassCryptoSpec (main, spec) where

import Crypto.Error (CryptoFailable(..))
import Data.ByteString.Base16 (encode, decode)
import Data.Text (Text)
import Test.Hspec

import PassCrypto.PassCrypto

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "encryptWithMasterKey" $ do
    it "encrypts plain text with a key" $ do
      let cfCipherText = encryptWithMasterKey testKey testSecret
      cfCipherText `shouldSatisfy` isCryptoPassed
      let CryptoPassed ct = cfCipherText
      ct `shouldBe` testCipherText

  describe "decryptWithMasterKey" $ do
    it "decrypts a cipher text with a key" $ do
      let cfPlainText = decryptWithMasterKey testKey testCipherText
      cfPlainText `shouldSatisfy` isCryptoPassed
      let CryptoPassed pt = cfPlainText
      pt `shouldBe` testSecret


isCryptoPassed :: CryptoFailable a -> Bool
isCryptoPassed (CryptoPassed _) = True
isCryptoPassed _ = False

testCipherText :: CipherText
testCipherText = CipherText $ fst . decode $ "ff792cda3b66783c9272734243bf997556d4d1fac019397f169255a6a18d83ba5235c34e0ac6f6e537b3d82729738355"

testKey :: MasterKey
testKey = MasterKey "can't touch this vault 7"

testSecret :: Text
testSecret = "I occasionally enjoy The Weeknd and J-Beibs"
