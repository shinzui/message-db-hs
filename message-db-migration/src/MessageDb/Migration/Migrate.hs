module MessageDb.Migration.Migrate (migrate, executeV130Migration) where

import Hasql.Session (Session)
import Hasql.Transaction (Transaction, sql)
import Hasql.Transaction.Sessions
import MessageDb.Migration.V130Migration (migrationSql)

executeV130Migration :: Transaction ()
executeV130Migration = mapM_ sql migrationSql

migrate :: Session ()
migrate = transaction Serializable Write executeV130Migration
