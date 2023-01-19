module MessageDb.Db.TastySetup (withPool) where

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Database.PostgreSQL.Simple.Options (Options (..))
import Database.Postgres.Temp.Tasty (withTmpPostgresPool)
import Hasql.Pool qualified as P
import Hasql.Session (run)
import MessageDb.Migration.Migrate (migrate)
import Test.Tasty.Runners (TestTree)

withPool :: (IO P.Pool -> TestTree) -> TestTree
withPool = withTmpPostgresPool "v130" addMessageStoreOptions (run migrate >=> either throwIO pure)

addMessageStoreOptions :: Options -> Options
addMessageStoreOptions opts =
  opts
    { options = pure "-cmessage_store.sql_condition=on\\ -csearch_path=message_store,\"$user\",public"
    }
