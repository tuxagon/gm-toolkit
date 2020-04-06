{-# LANGUAGE OverloadedStrings          #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist (get, insert, delete)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT)

import Schema


localConnectionString :: ConnectionString
localConnectionString =
  "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres"

fetchPostgresConnection :: IO ConnectionString
fetchPostgresConnection =
  return localConnectionString

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

fetchProduct :: ConnectionString -> Int64 -> IO (Maybe Product)
fetchProduct connString productId =
  runAction connString (get (toSqlKey productId))

createProduct :: ConnectionString -> Product -> IO Int64
createProduct connString product =
  fromSqlKey <$> runAction connString (insert product)

deleteProduct :: ConnectionString -> Int64 -> IO ()
deleteProduct connString productId =
  runAction connString (delete productKey)
  where
    productKey :: Key Product
    productKey = toSqlKey productId

migrateDB :: ConnectionString -> IO ()
migrateDB connString =
  runAction connString (runMigration migrateAll)
