{-# LANGUAGE
    OverloadedStrings
  , LambdaCase
  , DeriveGeneric
#-}
module PassCrypto.PassCrypto where

import Crypto.Error (CryptoFailable(..))
import Crypto.Cipher.Types
import Crypto.Cipher.AES
import Crypto.Hash
import GHC.Generics
import Data.ByteArray (convert)
import Data.Either
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
-- import qualifief Data.Text

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

newtype MasterKey = MasterKey {mKey :: ByteString}
  deriving (Generic, Show)

newtype CipherText = CipherText {cTxt :: ByteString}
  deriving (Generic, Show)

masterKey :: Text -> MasterKey
masterKey = MasterKey . encodeUtf8

keyToSHA256Bytes :: ByteString -> ByteString
keyToSHA256Bytes b = convert ((hash b) :: Digest SHA256)

getCipherForKey :: MasterKey -> CryptoFailable AES256
getCipherForKey = cipherInit . keyToSHA256Bytes . mKey

encryptWithMasterKey :: MasterKey -> Text -> CryptoFailable CipherText
encryptWithMasterKey mk pt = do
  cAES <- getCipherForKey mk
  encrypt (encodeUtf8 pt) cAES

decryptWithMasterKey :: MasterKey -> CipherText -> CryptoFailable Text
decryptWithMasterKey mk ct = do
  cAES <- getCipherForKey mk
  plainBytes <- decrypt ct cAES
  return $ decodeUtf8 $ plainBytes

encrypt :: ByteString -> AES256 -> CryptoFailable CipherText
encrypt pt cipher = return $ CipherText $ ecbEncrypt cipher (padToBlockSize pt)
  where
    padToBlockSize bytes =
      if blockSizeOffset /= 16
        then padNulls blockSizeOffset bytes
        else bytes
      where
        blockSizeOffset = 16 - (B.length bytes) `mod` 16
        padNulls 0 bs = bs
        padNulls n bs = padNulls (n-1) (B.append bs $ B.singleton 0)

decrypt :: CipherText -> AES256 -> CryptoFailable ByteString
decrypt ct cipher= return $ removeNull (ecbDecrypt cipher $ cTxt ct)
  where
    removeNull bytes = B.foldr' (\x y -> if x == 0 then y else B.cons x y) B.empty bytes

testBlockCipher :: IO ()
testBlockCipher =
    case encryptAndDecrypt (masterKey "super") "secret" of
      CryptoFailed err -> print $ show err
      CryptoPassed text -> print $ show text


encryptAndDecrypt :: MasterKey -> Text -> CryptoFailable Text
encryptAndDecrypt mk pt =  encryptWithMasterKey mk pt >>= decryptWithMasterKey mk
