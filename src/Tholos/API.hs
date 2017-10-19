{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Tholos.API where

import           Data.Text       (Text)
import           Servant

import qualified Tholos.API.V0          as V0
import           Tholos.App.Transformer (AppT)

type API = "api" :> Header "Accept" Text :> SubRoutesAPI

type SubRoutesAPI = V0.API

api :: Proxy API
api = Proxy

subroutesAPI :: Proxy SubRoutesAPI
subroutesAPI = Proxy
