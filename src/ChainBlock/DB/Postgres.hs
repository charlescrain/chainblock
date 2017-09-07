{-# LANGUAGE Arrows              #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module ChainBlock.DB.Postgres where

import           Control.Arrow                 (returnA)
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
import           Data.Text                     hiding (length, map)
import           Database.PostgreSQL.Simple    (ConnectInfo (..), Connection,
                                                SqlError (..), close, connect)
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


databaseInterface :: Connection
                  -> (forall a . PGDB a -> m a )
                  -> IO (IDataBase PGDB m)
databaseInterface conn runDBInterface' =
  return IDataBase { queryAllUsers     = queryAllUsers'     conn
                   , queryUser         = queryUser'         conn
                   , insertUser        = insertUser'        conn
                   , updateUser        = updateUser'        conn
                   , deleteUser        = deleteUser'        conn

                   , queryWebsites     = queryWebsites'     conn
                   , queryWebsite      = queryWebsite'      conn
                   , insertWebsite     = insertWebsite'     conn
                   , updateWebsite     = updateWebsite'     conn
                   , deleteWebsite     = deleteWebsite'     conn

                   , queryCredentials  = queryCredentials'  conn
                   , insertCredentials = insertCredentials' conn
                   , updateCredentials = updateCredentials' conn
                   , deleteCredentials = deleteCredentials' conn
                   , runDBI           = runDBInterface'
                   }

-----------------------------------------------------
-- | runDBInterface Functions
-----------------------------------------------------

runDBInterfaceBZ :: PGDB a -> BZ a
runDBInterfaceBZ = undefined

runDBInterfaceIO :: PGDB a -> (ExceptT CBError IO) a
runDBInterfaceIO = ExceptT . flip runLoggingT logMsg  . runExceptT . runPGDB

-----------------------------------------------------
-- | Interface Implementation
-----------------------------------------------------

-----------------------------------------------------
---- | User
-----------------------------------------------------

queryAllUsers' :: Connection -> PGDB [User]
queryAllUsers' conn = do
  let src = "queryAllUsers'"
  $logInfoS src "querying all users"
  rows <- catch (liftIO $ runQuery conn queryUsers)
    (\ (err :: SomeException) -> throwError . Ex $ err)
  return $ map (\(uId :: Int, un :: Text) -> User (Username un)
                                   (UserId . toInteger . fromIntegral $ uId) )
               rows
  where
    queryUsers :: Query (Column P.PGInt4, Column P.PGText)
    queryUsers = proc () -> do
      row <- queryTable userTable -< ()
      returnA -< row

queryUser' :: Connection -> Username -> PGDB User
queryUser' conn un@(Username unText) = do
  let src = "queryUser'"
  $logInfoS src ("running on user " <> unText)
  rows <- catch (liftIO $ runQuery conn queryUserByName)
                (\ (err :: SomeException) -> throwError . Ex $ err)
  case rows of
    [(userId' :: Int, uName :: Text)] -> return $ User
                    (Username uName)
                    (UserId . toInteger . fromIntegral $ userId')
    [] -> do
      let errorMsg = "0 rows found for username" <> unUsername un
      $logWarnS src errorMsg
      throwError $ DatabaseError src errorMsg NoResults
    _ -> do
      let errorMsg = "found multiple rows for username " <> unUsername un
      $logErrorS src errorMsg
      throwError . Ex . SomeException $ DatabaseEx
           src
           errorMsg
           NotUnique
  where
    queryUserByName :: Query (Column P.PGInt4, Column P.PGText)
    queryUserByName = proc () -> do
      row@(_,uName) <- queryTable userTable -< ()
      restrict -< (P.pgStrictText unText .== uName)
      returnA -< row

insertUser' ::  Connection -> Username -> PGDB UserId
insertUser' conn un = do
  let src = "insertUser'"
      insertFields = [(Nothing, P.pgStrictText . unUsername $ un)]
  $logInfoS src ("inserting user" <> unUsername un)
  [(userId' :: Int, _ :: Text)] <- catch
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
  return (UserId . toInteger . fromIntegral $ userId')

updateUser' ::  Connection -> UserId -> Username -> PGDB ()
updateUser' _conn _uId' _un = undefined

deleteUser' ::  Connection -> UserId -> PGDB ()
deleteUser' _conn _uId' = undefined

-----------------------------------------------------
---- | Website
-----------------------------------------------------

queryWebsites' :: Connection -> UserId -> PGDB [Website]
queryWebsites' = undefined

queryWebsite' :: Connection -> WebsiteId -> PGDB Website
queryWebsite' = undefined

insertWebsite' :: Connection
               -> UserId
               -> WebsiteURL
               -> WebsiteName
               -> PGDB WebsiteId
insertWebsite' = undefined

updateWebsite' :: Connection
               -> WebsiteId
               -> WebsiteURL
               -> WebsiteName
               -> PGDB ()
updateWebsite' = undefined

deleteWebsite' :: Connection -> WebsiteId -> PGDB ()
deleteWebsite' = undefined

-----------------------------------------------------
---- | Credentials
-----------------------------------------------------
queryCredentials' :: Connection -> UserId -> WebsiteId -> PGDB [Credentials]
queryCredentials' = undefined

insertCredentials' :: Connection
                   -> UserId
                   -> EncryptedPassword
                   -> WebUsername
                   -> PGDB CredentialsId
insertCredentials' = undefined

updateCredentials' :: Connection
                   -> CredentialsId
                   -> EncryptedPassword
                   -> WebUsername
                   -> PGDB ()
updateCredentials' = undefined

deleteCredentials' :: Connection -> CredentialsId -> PGDB ()
deleteCredentials' = undefined

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
  return ConnectInfo { connectHost = host
                       , connectPort = read port
                       , connectUser = dbUser
                       , connectPassword = dbPassword
                       , connectDatabase = dbName
                       }
