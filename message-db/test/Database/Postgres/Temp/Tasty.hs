module Database.Postgres.Temp.Tasty (withTmpPostgresPool) where

import Control.Exception (throwIO)
import Control.Exception.Base (bracket)
import Data.ByteString
import Database.PostgreSQL.Simple.Options (Options (..), toConnectionString)
import Database.Postgres.Temp
  ( DB,
    cacheAction,
    defaultConfig,
    startConfig,
    stop,
    toConnectionOptions,
  )
import Hasql.Connection (Connection, acquire, release)
import Hasql.Pool qualified as P
import System.FilePath ((</>))
import Test.Tasty (withResource)
import Test.Tasty.Runners (TestTree)

type UpdatePostgresConnectionConfig = Options -> Options

-- |
withTmpPostgresPool ::
  -- | Location of the subdirectory data cache
  FilePath ->
  -- | Function to update the postgres 'Options'
  UpdatePostgresConnectionConfig ->
  -- | The migration to execute before caching
  (Connection -> IO ()) ->
  -- | Callback with the pool to use for running the tests
  (IO P.Pool -> TestTree) ->
  TestTree
withTmpPostgresPool cachePath connOpts migrate t =
  withResource start stop f -- acquire a temp postgres DB
  where
    f db = withResource (db >>= \db' -> P.acquire 10 (Just 60) (connectionString connOpts db')) P.release t
    start =
      either throwIO pure =<< do
        migratedConfig <-
          either throwIO pure
            =<< cacheAction
              (mkTmpPgCachePath cachePath)
              (flip (withTmpDbConnection connOpts) migrate)
              defaultConfig
        startConfig migratedConfig

withTmpDbConnection :: UpdatePostgresConnectionConfig -> DB -> (Connection -> IO a) -> IO a
withTmpDbConnection connOpts db f = do
  let connStr = connectionString connOpts db
  bracket (either (throwIO . userError . show) pure =<< acquire connStr) release f

connectionString :: UpdatePostgresConnectionConfig -> DB -> ByteString
connectionString connOpts db =
  toConnectionString
    (connOpts (toConnectionOptions db))

mkTmpPgCachePath :: FilePath -> FilePath
mkTmpPgCachePath = (".tmp-postgres" </>)
