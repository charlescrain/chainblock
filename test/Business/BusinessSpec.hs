{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Business.BusinessSpec (main, spec) where

import           Control.Monad.Except          (ExceptT (..), runExceptT)
import           Data.Either
import           Data.List                     (elem)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import           Database.PostgreSQL.Simple    (ConnectInfo (..), Connection,
                                                close, connect)
import           System.Environment            (getEnv)
import           Test.Hspec
import           Test.QuickCheck.Arbitrary     (Arbitrary (..))
import           Test.QuickCheck.Gen           (generate)

import           ChainBlock.Business
import           ChainBlock.Business.Interface
import           ChainBlock.Business.Types
import           ChainBlock.DB.Interface
import           ChainBlock.DB.Postgres
import           ChainBlock.DB.Postgres.Setup
import           ChainBlock.DB.Postgres.Types  (PGDB (..))
import           ChainBlock.DB.Types
import           ChainBlock.Errors

main :: IO ()
main = hspec spec

spec :: Spec
spec = bzSpec


bzSpec :: Spec
bzSpec = do
    dbi <- runIO generateDBI
    bzi <- runIO $ businessInterface runBusinessInterfaceIO dbi
    runBusniessSpecs bzi
  where
    runBusniessSpecs bzi = do
      userSpec bzi
      -- websiteSpec dbi
      -- credntialsSpec dbi

userSpec :: IBusinessFunctions BZ (ExceptT CBError IO)  -> Spec
userSpec dbi =
  describe "User Spec" $ do
    it "should get a list of users" $
      pendingWith "unimplemented"
    it "should post a user and get an id" $
      pendingWith "unimplemented"
    it "should post a user and handle duplciate key error" $
      pendingWith "unimplemented"

websiteSpec :: IDataBase IO (ExceptT CBError IO)  -> Spec
websiteSpec dbi = describe "Website Spec" $ do
    it "should get a list of websites" $
      pendingWith "unimplemented"
    it "should post a website and get an id" $
      pendingWith "unimplemented"


------------------------------------------------------------------------------
--Spec Utils
------------------------------------------------------------------------------


generateDBI :: IO (IDataBase IO (ExceptT CBError IO))
generateDBI = undefined
