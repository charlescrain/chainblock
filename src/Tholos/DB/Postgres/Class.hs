
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tholos.DB.Postgres.Class where

import           Database.PostgreSQL.Simple (Connection)

import Tholos.App.Transformer

class PGConn m where
  getConn :: m Connection


instance PGConn AppT where
  getConn = undefined
