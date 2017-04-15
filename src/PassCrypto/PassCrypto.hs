{-# LANGUAGE
    OverloadedStrings
  , LambdaCase
  , DeriveGeneric
#-}
module PassCrypto.PassCrypto where

import Debug.Trace

import Crypto.Error (CryptoFailable(..))
import Crypto.Cipher.Types
import Crypto.Cipher.AES
import Crypto.Hash
import GHC.Generics
import Data.Binary (Binary, Word8)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.ByteArray (convert)
import Data.Either
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
-- import qualifief Data.Text

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

newtype MasterKey = MasterKey {mKey :: ByteString}
  deriving (Generic, Show , Eq)

newtype CipherText = CipherText {cTxt :: ByteString}
  deriving (Generic, Show, Eq)

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
encrypt pt cipher = do
  let (padSizeBS, leftover) =  B.splitAt 1 (padToBlockSize pt)
  return $
    CipherText $ cbcEncrypt cipher nullIV (padToBlockSize pt)
  where
    padToBlockSize bytes =
      let
        prependedBytes = (prependPadOffset (blockSizeOffset bytes - 1) bytes)
        padOffset = blockSizeOffset prependedBytes
      in padNulls padOffset prependedBytes
      where
        blockSizeOffset bs = 16 - (B.length bs) `mod` 16
        padNulls 0 bs = bs
        padNulls 16 bs = bs
        padNulls n bs = padNulls (n-1) (B.append bs $ B.singleton 0)
        prependPadOffset n bs =
          (B.singleton  (fromIntegral n))
          `B.append`
          bs

decrypt :: CipherText -> AES256 -> CryptoFailable ByteString
decrypt ct cipher = do
  return $
    removeNull $ cbcDecrypt cipher nullIV (cTxt ct)
  where
    removeNull bytes =
      let
        (padSizeBS, leftover) =  B.splitAt 1 bytes
        padSize = fromIntegral ((Binary.decode (ByteString.Lazy.fromStrict padSizeBS))::Word8)
      in B.take (B.length leftover -  padSize) leftover

testBlockCipher :: IO ()
testBlockCipher =
    case encryptAndDecrypt (masterKey "super") "secret" of
      CryptoFailed err -> do
        print $ "Fucking error!!!"
        print $ show err
      CryptoPassed text -> print $ show text


encryptAndDecrypt :: MasterKey -> Text -> CryptoFailable Text
encryptAndDecrypt mk pt =  encryptWithMasterKey mk pt >>= decryptWithMasterKey mk
