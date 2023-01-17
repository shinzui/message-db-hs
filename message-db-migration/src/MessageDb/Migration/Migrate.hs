module MessageDb.Migration.Migrate (executeV130Migration) where

import Hasql.Transaction (Transaction, sql)
import MessageDb.Migration.V130Migration (migrationSql)

executeV130Migration :: Transaction ()
executeV130Migration = mapM_ sql migrationSql
