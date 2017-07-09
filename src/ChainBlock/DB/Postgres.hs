{-# LANGUAGE Arrows              #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module ChainBlock.DB.Postgres where

import           Control.Arrow                 (returnA)
import           Control.Exception.Base        (IOException)
import           Control.Monad.Catch           (Exception (..),
                                                SomeException (..), catch,
                                                throwM, try)
import           Control.Monad.Except          (ExceptT (..), runExceptT,
                                                throwError)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Logger
import           Data.ByteString.Lazy          (toStrict)
import           Data.ByteString.Lazy.UTF8     (fromString)
import           Data.Monoid                   ((<>))
import           Data.Text                     hiding (length)
import           Database.PostgreSQL.Simple    (ConnectInfo (..), Connection,
                                                connect)
import           Database.PostgreSQL.Simple    (SqlError (..))
import           Opaleye                       (queryTable, restrict, (.==))
import           Opaleye.Column                (Column)
import           Opaleye.Manipulation          (runInsertManyReturning)
import qualified Opaleye.PGTypes               as P
import           Opaleye.QueryArr              (Query)
import           Opaleye.RunQuery              (runQuery)
import           System.Environment            (getEnv)

import           ChainBlock.Business.Types     (BZ)
import           ChainBlock.DB.Interface
import           ChainBlock.DB.Postgres.Setup  (createDBIfNeeded)
import           ChainBlock.DB.Postgres.Tables
import           ChainBlock.DB.Postgres.Types  (PGDB (..))
import           ChainBlock.DB.Types
import           ChainBlock.Errors
import           ChainBlock.Logging


databaseInterface :: (forall a . PGDB a -> m a )
                  -> IO (IDataBase PGDB m)
databaseInterface runDBInterface' = do
  connInfo <- buildConnectInfo
  let dbName = connectDatabase connInfo
  _ <- createDBIfNeeded connInfo dbName
  conn <- connect connInfo
  return IDataBase { queryAllUsers           = queryAllUsers' conn
                   , queryUser               = queryUser' conn
                   , insertUser              = insertUser' conn
                   , queryWebsite            = queryWebsite' conn
                   , queryWebsiteCredentials = queryWebsiteCredentials' conn
                   , runDBI                  = runDBInterface'
                   }

-----------------------------------------------------
-- | runDBInterface Functions
-----------------------------------------------------

runDBInterfaceBZ :: PGDB a -> BZ a
runDBInterfaceBZ = undefined

runDBInterfaceIO :: PGDB a -> (ExceptT CBError IO) a
runDBInterfaceIO f = ExceptT . flip runLoggingT logMsg  . runExceptT . runPGDB $ f

-----------------------------------------------------
-- | Interface Implementation
-----------------------------------------------------

queryAllUsers' :: Connection -> PGDB [User]
queryAllUsers' =  undefined

queryUser' :: Connection -> Username -> PGDB User
queryUser' conn un@(Username unText) = do
  let src = "queryUser'"
  $logInfoS src ("running on user" <> unText)
  rows <- catch (liftIO $ runQuery conn queryUserByName)
    (\ (err :: IOException) -> undefined)
  case rows of
    [(uId :: Int, uName :: Text)] -> return $ User
                    (Username uName)
                    (UserId . toInteger . fromIntegral $ uId)
    [] -> do
      let errorMsg = ("0 rows found for username" <> unUsername un)
      $logWarnS src errorMsg
      throwError $ DatabaseError src errorMsg NoResults
    _ -> do
      let errorMsg = ("found multiple rows for username " <> unUsername un)
      $logErrorS src errorMsg
      throwError . Ex . SomeException $ DatabaseEx
           src
           errorMsg
           NotUnique
  where
    queryUserByName :: Query (Column P.PGInt4, Column P.PGText)
    queryUserByName = proc () -> do
      row@(_,uName) <- queryTable userTable -< ()
      restrict -< (P.pgStrictText (unText) .== uName)
      returnA -< row

insertUser' ::  Connection -> Username -> PGDB UserId
insertUser' conn un = do
  let src = "insertUser'"
      insertFields = [(Nothing, P.pgStrictText . unUsername $ un)]
  $logInfoS src ("inserting user" <> unUsername un)
  [(id :: Int, _ :: Text)] <- catch
    (liftIO $ runInsertManyReturning conn userTable insertFields id)
    (\(err :: SqlError) -> case sqlState err of
      "23505" -> do
        let errorMsg = "User with name " <> unUsername un <> " already exists"
        $logWarnS src errorMsg
        throwError $ DatabaseError src errorMsg DuplicateKeyViolation
      _ -> do
        $logErrorS src ("SqlError " <> (pack . show $ err))
        throwError . Ex . SomeException $ err
    )
  return $ UserId . toInteger . fromIntegral $ id


queryWebsite' :: Connection -> UserId -> PGDB [Website]
queryWebsite' = undefined

queryWebsiteCredentials' :: Connection -> UserId -> WebsiteId -> PGDB [WebsiteCredentials]
queryWebsiteCredentials' = undefined


-----------------------------------------------------
-- | Helper Funcitons
-----------------------------------------------------


buildConnectInfo :: IO ConnectInfo
buildConnectInfo  = do
  host       <- getEnv "PG_HOST"
  port       <- getEnv "PG_PORT"
  dbName     <- getEnv "PG_DBNAME"
  dbUser     <- getEnv "PG_USER"
  dbPassword <- getEnv "PG_PASSWORD"
  return $ ConnectInfo { connectHost = host
                       , connectPort = read port
                       , connectUser = dbUser
                       , connectPassword = dbPassword
                       , connectDatabase = dbName
                       }

