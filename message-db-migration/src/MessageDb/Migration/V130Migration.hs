{-# LANGUAGE TemplateHaskell #-}

module MessageDb.Migration.V130Migration (migrationFiles, migrationSql) where

import Data.ByteString
import Data.FileEmbed (embedDir)
import Data.Maybe (fromJust)

files :: [(FilePath, ByteString)]
files = $(embedDir "database/v1.3.0/database")

migrationSql :: [ByteString]
migrationSql = getSql <$> migrationFiles
  where
    getSql f = fromJust $ lookup f files

migrationFiles :: [FilePath]
migrationFiles =
  [ "roles/message-store.sql",
    "schema/message-store.sql",
    "extensions/pgcrypto.sql",
    "tables/messages.sql",
    -- functions
    "types/message.sql",
    "functions/message-store-version.sql",
    "functions/hash-64.sql",
    "functions/acquire-lock.sql",
    "functions/category.sql",
    "functions/is-category.sql",
    "functions/id.sql",
    "functions/cardinal-id.sql",
    "functions/stream-version.sql",
    "functions/write-message.sql",
    "functions/get-stream-messages.sql",
    "functions/get-category-messages.sql",
    "functions/get-last-stream-message.sql",
    -- indexes
    "indexes/messages-id.sql",
    "indexes/messages-stream.sql",
    "indexes/messages-category.sql",
    -- views
    "views/stream-summary.sql",
    "views/type-summary.sql",
    "views/stream-type-summary.sql",
    "views/type-stream-summary.sql",
    "views/category-type-summary.sql",
    "views/type-category-summary.sql",
    -- privileges
    "privileges/schema.sql",
    "privileges/table.sql",
    "privileges/sequence.sql",
    "privileges/functions.sql",
    "privileges/views.sql"
  ]
