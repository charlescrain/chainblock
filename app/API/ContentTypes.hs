{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module API.ContentTypes where

import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Aeson.Types           (parseEither)
import           Data.Attoparsec.ByteString (endOfInput, parseOnly)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Text                  as T
import           Data.Typeable
import           Network.HTTP.Media         ((//), (/:))
import           Servant

data JSONAPI deriving Typeable

instance Accept JSONAPI where
  contentType _ = ("application" :: ByteString) // ("vnd.api+json" :: ByteString)
instance ToJSON a => MimeRender JSONAPI a where
  mimeRender _ = encode
instance FromJSON a => MimeUnrender JSONAPI a where
  mimeUnrender _ = eitherDecodeLenient

eitherDecodeLenient :: FromJSON a => BSL.ByteString -> Either String a
eitherDecodeLenient input = do
    v :: Value <- parseOnly (value <* endOfInput) (BSL.toStrict input)
    parseEither parseJSON v
