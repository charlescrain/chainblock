{-# LANGUAGE Arrows              #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Tholos.DB.Postgres where

import           Control.Arrow              (returnA)
import           Control.Monad.Catch        (Exception (..), SomeException (..),
                                             catch, throwM, try)
import           Control.Monad.Except       (MonadError(..), ExceptT (..), runExceptT,
                                             throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import           Control.Monad.Logger
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (toStrict)
import           Data.ByteString.Lazy.UTF8  (fromString)
import           Data.Monoid                ((<>))
import           Data.Text                  hiding (length, map)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             SqlError (..), close, connect)
import           Opaleye                    (queryTable, restrict, (.==))
import           Opaleye.Column             (Column)
import           Opaleye.Manipulation       (runDelete, runInsertManyReturning,
                                             runUpdate)
import qualified Opaleye.PGTypes            as P
import           Opaleye.QueryArr           (Query)
import           Opaleye.RunQuery           (runQuery)
import           System.Environment         (getEnv)

import qualified Tholos.API.Class as API
import  Tholos.App.Transformer
import           Tholos.DB.Interface
import           Tholos.DB.Postgres.Setup   (createDBIfNeeded)
import           Tholos.DB.Postgres.Tables
import           Tholos.DB.Postgres.Class
import           Tholos.DB.Postgres.Types   (PGDB (..))
import           Tholos.Types
import           Tholos.Errors
import           Tholos.Logging
import           Tholos.Monad


createInterface :: Connection
                  -> (forall a . PGDB a -> m a )
                  -> IO (IDataBase m)
createInterface conn runInterface =
  return IDataBase { -- queryAllUsers = runInterface $ queryAllUsers' conn
                    queryUser     = runInterface . queryUser' conn
                   , insertUser    = runInterface . insertUser'   conn
                   , updateUser    = \x y -> runInterface $ updateUser' conn  x y
                   , deleteUser    = runInterface . deleteUser'   conn

                   , queryWebsites = runInterface . queryWebsites' conn
                   , queryWebsite  = runInterface . queryWebsite'  conn
                   , insertWebsite = \w x y -> runInterface $ insertWebsite' conn w x y
                   , updateWebsite = \w x y -> runInterface $ updateWebsite' conn w x y
                   , deleteWebsite = runInterface . deleteWebsite' conn

                   , queryAllUserCredentials = runInterface . queryAllUserCredentials' conn
                   , queryCredentials        = runInterface . queryCredentials'        conn
                   , insertCredentials       = \w x y z -> runInterface $ insertCredentials' conn w x y z
                   , updateCredentials       = \w x y -> runInterface $ updateCredentials' conn w x y
                   , deleteCredentials       = runInterface . deleteCredentials'       conn
                   }

-----------------------------------------------------
-- | runDBInterface Functions
-----------------------------------------------------

runInterfaceExceptT :: PGDB a -> (ExceptT TholosError IO) a
runInterfaceExceptT = ExceptT . runCommonT . runPGDB

runInterfaceCommonT :: PGDB a -> CommonT a
runInterfaceCommonT = runPGDB

-----------------------------------------------------
-- | Instances 
-----------------------------------------------------

-----------------------------------------------------
---- | User
-----------------------------------------------------

queryAllUsers :: ( PGConn m
                 , MonadError TholosError m
                 , MonadThrow m
                 , MonadCatch m
                 , MonadIO m
                 , MonadLogger m
                 ) => m [User]
queryAllUsers = do
  let src = "queryAllUsers'"
  conn <- getConn
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
queryUser' conn (Username un) = do
  let src = "queryUser'"
  $logInfoS src ("running on user " <> un)
  rows <- catch (liftIO $ runQuery conn queryUserByName)
                (\ (err :: SomeException) -> throwError . Ex $ err)
  case rows of
    [(userId' :: Int, uName :: Text)] -> return $ User
                    (Username uName)
                    (UserId . toInteger . fromIntegral $ userId')
    [] -> do
      let errorMsg = "0 rows found for username " <> un
      $logWarnS src errorMsg
      throwError $ DatabaseError src errorMsg NoResults
    _ -> do
      let errorMsg = "found multiple rows for username " <> un
      $logErrorS src errorMsg
      throwError . Ex . SomeException $ DatabaseEx
           src
           errorMsg
           NotUnique
  where
    queryUserByName :: Query (Column P.PGInt4, Column P.PGText)
    queryUserByName = proc () -> do
      row@(_,uName) <- queryTable userTable -< ()
      restrict -< (P.pgStrictText un .== uName)
      returnA -< row

insertUser' ::  Connection -> Username -> PGDB UserId
insertUser' conn (Username un) = do
  let src = "insertUser'"
      insertFields = [(Nothing, P.pgStrictText  un)]
  $logInfoS src ("inserting user " <> un)
  [(userId' :: Int, _ :: Text)] <- catch
    (liftIO $ runInsertManyReturning conn userTable insertFields id)
    (\(err :: SqlError) -> case sqlState err of
      "23505" -> do
        let errorMsg = "User with name " <> un <> " already exists"
        $logErrorS src errorMsg
        throwError $ DatabaseError src errorMsg DuplicateKeyViolation
      _ -> do
        $logErrorS src ("SqlError " <> (pack . show $ err))
        throwError . Ex . SomeException $ err
    )
  return (UserId . toInteger . fromIntegral $ userId')

updateUser' ::  Connection -> UserId -> Username -> PGDB ()
updateUser' conn (UserId uId') (Username un) = do
  let src = "updateUser'"
  $logInfoS src ("updating user " <> un)
  updatedRowCount <- catch
         (liftIO $ runUpdate
                     conn
                     userTable
                     (\ (id',_) -> (Just id',P.pgStrictText un))
                     (\ (id',_) -> (id' .==) . P.pgInt4 $ fromInteger uId'))
         (\(err :: SqlError) -> do
             $logErrorS src ("SqlError " <> (pack . show $ err))
             throwError . Ex . SomeException $ err)
  $logDebugS src ("Updated " <> pack (show updatedRowCount) <> "rows.")
  if updatedRowCount == 0
    then do
        let errorMsg = pack ("No record with id found. id=" <> show uId' )
        $logErrorS src errorMsg
        throwError $ DatabaseError src errorMsg NoRowsAltered
    else return ()

deleteUser' ::  Connection -> UserId -> PGDB ()
deleteUser' conn (UserId uId') = do
  let src = "deleteUser'"
  $logInfoS src ("deleting user with id " <> pack (show uId'))
  deletedRows <- catch
         (liftIO $ runDelete
                     conn
                     userTable
                     (\ (id',_) -> (id' .==) . P.pgInt4 $ fromInteger uId'))
         (\(err :: SqlError) -> do
             $logErrorS src ("SqlError " <> (pack . show $ err))
             throwError . Ex . SomeException $ err)
  if deletedRows == 0
    then do
        let errorMsg = pack ("No record with id found. id=" <> show uId' )
        $logErrorS src errorMsg
        throwError $ DatabaseError src errorMsg NoRowsAltered
    else return ()

-----------------------------------------------------
---- | Website
-----------------------------------------------------

queryWebsites' :: Connection -> UserId -> PGDB [WebsiteDetails]
queryWebsites' conn (UserId uId') = do
  let src = "queryWebsites'"
  $logInfoS src $ pack ("querying websites for user with id: " <> show uId')
  rows <- catch (liftIO $ runQuery conn queryWeb)
    (\ (err :: SomeException) -> throwError . Ex $ err)
  return $ map (\(wId :: Int, wURL :: Text, wName :: Text, userId' :: Int) ->
                    WebsiteDetails { websiteURL  = WebsiteURL wURL
                                   , websiteName = WebsiteName wName
                                   , websiteId   = WebsiteId . toInteger . fromIntegral $ wId
                                   , userId      = UserId . toInteger . fromIntegral $ userId'
                                   })
               rows
  where
    queryWeb :: Query (Column P.PGInt4, Column P.PGText, Column P.PGText, Column P.PGInt4)
    queryWeb = proc () -> do
      row@(_,_,_,uId'') <- queryTable websiteTable -< ()
      restrict -< (P.pgInt4 (fromIntegral uId') .== uId'')
      returnA -< row

queryWebsite' :: Connection -> WebsiteId -> PGDB WebsiteDetails
queryWebsite' conn (WebsiteId wId') = do
  let src = "queryWebsite'"
  $logInfoS src ("running on websiteId " <> pack (show wId'))
  rows <- catch (liftIO $ runQuery conn queryWebsiteById)
                (\ (err :: SomeException) -> throwError . Ex $ err)
  case rows of
      [( wId' :: Int
       , wURL' :: Text
       , wName' :: Text
       , uId' :: Int)]  -> return WebsiteDetails { websiteURL  = WebsiteURL wURL'
                                                 , websiteName = WebsiteName wName'
                                                 , websiteId   = WebsiteId . toInteger . fromIntegral $ wId'
                                                 , userId      = UserId . toInteger . fromIntegral $ uId'
                                                 }
      [] -> do
        let errorMsg = "0 rows found for websiteId " <> pack (show wId')
        $logWarnS src errorMsg
        throwError $ DatabaseError src errorMsg NoResults
      _ -> do
        let errorMsg = "found multiple rows for websiteId " <> pack (show wId')
        $logErrorS src errorMsg
        throwError . Ex . SomeException $ DatabaseEx
             src
             errorMsg
             NotUnique
  where
    queryWebsiteById :: Query (Column P.PGInt4, Column P.PGText, Column P.PGText, Column P.PGInt4)
    queryWebsiteById = proc () -> do
      row@(wid',_,_,_) <- queryTable websiteTable -< ()
      restrict -< (P.pgInt4 (fromIntegral wId') .== wid')
      returnA -< row

insertWebsite' :: Connection
               -> UserId
               -> WebsiteURL
               -> WebsiteName
               -> PGDB WebsiteId
insertWebsite' conn (UserId uId') (WebsiteURL wURL) (WebsiteName wName) = do
  let src = "insertWebsite'"
      insertFields = [(Nothing, P.pgStrictText wURL, P.pgStrictText wName, P.pgInt4 . fromIntegral $ uId')]
  $logInfoS src ( "inserting website,"
               <> " userId: " <> (pack . show $ uId')
               <> " websiteURL: " <> wURL
               <> " websiteName: " <> wName)

  -- TODO: use case statement
  [( wId' :: Int
   , _ :: Text
   , _ :: Text
   , _ :: Int)] <- catch
                      (liftIO $ runInsertManyReturning conn websiteTable insertFields id)
                      (\(err :: SqlError) -> do
                          $logErrorS src ("SqlError " <> (pack . show $ err))
                          throwError . Ex . SomeException $ err)
  return (WebsiteId . toInteger . fromIntegral $ wId')

updateWebsite' :: Connection
               -> WebsiteId
               -> WebsiteURL
               -> WebsiteName
               -> PGDB ()
updateWebsite' conn
               (WebsiteId wId')
               (WebsiteURL wURL)
               (WebsiteName wName) = do
  let src = "updateWebsite'"
  $logInfoS src ("updating website with id " <> pack (show wId'))
  updatedRowCount <- catch
         (liftIO $ runUpdate
                     conn
                     websiteTable
                     (\ (_,_,_,uId') -> ( Nothing
                                        , P.pgStrictText wURL
                                        , P.pgStrictText wName
                                        , uId'))
                     (\ (id',_,_,_) -> (id' .==) . P.pgInt4 $ fromInteger wId'))
         (\(err :: SqlError) -> do
             $logErrorS src ("SqlError " <> (pack . show $ err))
             throwError . Ex . SomeException $ err)
  $logDebugS src ("Updated " <> pack (show updatedRowCount) <> "rows.")
  if updatedRowCount == 0
    then do
        let errorMsg = pack ("No record with id found. id=" <> show wId' )
        $logErrorS src errorMsg
        throwError $ DatabaseError src errorMsg NoRowsAltered
    else return ()

deleteWebsite' :: Connection -> WebsiteId -> PGDB ()
deleteWebsite' conn (WebsiteId wId') = do
  let src = "deleteWebsite'"
  $logInfoS src ("deleting website with id " <> pack (show wId'))
  deletedRows <- catch
         (liftIO $ runDelete
                     conn
                     websiteTable
                     (\ (id',_,_,_) -> (id' .==) . P.pgInt4 $ fromInteger wId'))
         (\(err :: SqlError) -> do
             $logErrorS src ("SqlError " <> (pack . show $ err))
             throwError . Ex . SomeException $ err)
  if deletedRows == 0
    then do
        let errorMsg = pack ("No record with id found. id=" <> show wId' )
        $logErrorS src errorMsg
        throwError $ DatabaseError src errorMsg NoRowsAltered
    else return ()

-----------------------------------------------------
---- | Credentials
-----------------------------------------------------
queryAllUserCredentials' :: Connection -> UserId -> PGDB [Credentials]
queryAllUserCredentials' conn (UserId uId') = do
  let src = "queryAllUserCredentials'"
  $logInfoS src $ pack ("querying credentials for user with id: " <> show uId')
  rows <- catch (liftIO $ runQuery conn queryCred)
    (\ (err :: SomeException) -> throwError . Ex $ err)
  return $ map (\ ( cId :: Int
                  , wName :: Text
                  , ep :: ByteString
                  , wId' :: Int
                  , userId' :: Int) ->
                    Credentials { credId = CredentialsId . toInteger . fromIntegral $ cId
                                , webUsername = WebUsername wName
                                , encPassword = EncryptedPassword ep
                                , webId = WebsiteId . toInteger . fromIntegral $ wId'
                                , credUserId = UserId . toInteger . fromIntegral $ userId'
                                })
               rows
  where
    queryCred :: Query (Column P.PGInt4, Column P.PGText, Column P.PGBytea, Column P.PGInt4, Column P.PGInt4)
    queryCred = proc () -> do
      row@(_,_,_,_,uId'') <- queryTable credentialsTable -< ()
      restrict -< (P.pgInt4 (fromIntegral uId') .== uId'')
      returnA -< row

queryCredentials'  :: Connection -> CredentialsId -> PGDB Credentials
queryCredentials' conn (CredentialsId cId') = do
  let src = "queryCredentials'"
  $logInfoS src ("running on credentialsId " <> pack (show cId'))
  rows <- catch (liftIO $ runQuery conn queryCredneitalsById)
                (\ (err :: SomeException) -> throwError . Ex $ err)
  case rows of
      [( cId'' :: Int
       , wUsername' :: Text
       , ePass' :: ByteString
       , wId' :: Int
       , uId' :: Int)]  -> return Credentials { credId = CredentialsId . toInteger . fromIntegral $ cId''
                                              , webUsername = WebUsername wUsername'
                                              , encPassword = EncryptedPassword ePass'
                                              , webId = WebsiteId . toInteger . fromIntegral $ wId'
                                              , credUserId = UserId . toInteger . fromIntegral $ uId'
                                              }
      [] -> do
        let errorMsg = "0 rows found for CredentialsId " <> pack (show cId')
        $logWarnS src errorMsg
        throwError $ DatabaseError src errorMsg NoResults
      _ -> do
        let errorMsg = "found multiple rows for CredentialsId " <> pack (show cId')
        $logErrorS src errorMsg
        throwError . Ex . SomeException $ DatabaseEx
             src
             errorMsg
             NotUnique
  where
    queryCredneitalsById :: Query (Column P.PGInt4, Column P.PGText, Column P.PGBytea, Column P.PGInt4, Column P.PGInt4)
    queryCredneitalsById = proc () -> do
      row@(cid',_,_,_,_) <- queryTable credentialsTable -< ()
      restrict -< (P.pgInt4 (fromIntegral cId') .== cid')
      returnA -< row

insertCredentials' :: Connection
                   -> UserId
                   -> WebsiteId
                   -> EncryptedPassword
                   -> WebUsername
                   -> PGDB CredentialsId
insertCredentials' conn
                   (UserId uId')
                   (WebsiteId wId')
                   (EncryptedPassword ePass)
                   (WebUsername wUsername) = do
  let src = "insertCredentials'"
      insertFields = [( Nothing
                      , P.pgStrictText wUsername
                      , P.pgStrictByteString ePass
                      , P.pgInt4 . fromIntegral $ wId'
                      , P.pgInt4 . fromIntegral $ uId')]
  $logInfoS src ( "inserting credentials,"
               <> " userId: " <> (pack . show $ uId')
               <> " websiteId: " <> (pack . show $ wId')
               <> " websiteURL: " <> wUsername)
  [( cId' :: Int
   , _ :: Text
   , _ :: ByteString
   , _ :: Int
   , _ :: Int)] <- catch
                      (liftIO $ runInsertManyReturning conn credentialsTable insertFields id)
                      (\(err :: SqlError) -> do
                          $logErrorS src ("SqlError " <> (pack . show $ err))
                          throwError . Ex . SomeException $ err)
  return (CredentialsId . toInteger . fromIntegral $ cId')

updateCredentials' :: Connection
                   -> CredentialsId
                   -> EncryptedPassword
                   -> WebUsername
                   -> PGDB ()
updateCredentials' conn
                   (CredentialsId cId')
                   (EncryptedPassword ep)
                   (WebUsername wName) = do
  let src = "updateCredentials'"
  $logInfoS src ("updating credentials with id " <> pack (show cId'))
  updatedRowCount <- catch
         (liftIO $ runUpdate
                     conn
                     credentialsTable
                     (\ (_,_,_,wId',uId') -> ( Nothing
                                             , P.pgStrictText wName
                                             , P.pgStrictByteString ep
                                             , wId'
                                             , uId'))
                     (\ (id',_,_,_,_) -> (id' .==) . P.pgInt4 $ fromInteger cId'))
         (\(err :: SqlError) -> do
             $logErrorS src ("SqlError " <> (pack . show $ err))
             throwError . Ex . SomeException $ err)
  $logDebugS src ("Updated " <> pack (show updatedRowCount) <> "rows.")
  if updatedRowCount == 0
    then do
        let errorMsg = pack ("No record with id found. id=" <> show cId' )
        $logErrorS src errorMsg
        throwError $ DatabaseError src errorMsg NoRowsAltered
    else return ()
deleteCredentials' :: Connection -> CredentialsId -> PGDB ()
deleteCredentials' conn (CredentialsId cId') = do
  let src = "deleteCredentials'"
  $logInfoS src ("deleting credentials with id " <> pack (show cId'))
  deletedRows <- catch
         (liftIO $ runDelete
                     conn
                     credentialsTable
                     (\ (id',_,_,_,_) -> (id' .==) . P.pgInt4 $ fromInteger cId'))
         (\(err :: SqlError) -> do
             $logErrorS src ("SqlError " <> (pack . show $ err))
             throwError . Ex . SomeException $ err)
  if deletedRows == 0
    then do
        let errorMsg = pack ("No record with id found. id=" <> show cId' )
        $logErrorS src errorMsg
        throwError $ DatabaseError src errorMsg NoRowsAltered
    else return ()

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
