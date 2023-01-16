defaultMessageDbVersion := "v1.3.0"

format:
 ormolu --mode inplace --ghc-opt=-XImportQualifiedPost message-db/src/**/*.hs
 ormolu --mode inplace --ghc-opt=-XImportQualifiedPost,-XOverloadedRecordDot message-db/test/**/*.hs

build:
 cabal build all

get-message-db messageDbVersion=defaultMessageDbVersion:
  git subtree add --prefix "message-db-migration/database/{{messageDbVersion}}" --squash https://github.com/message-db/message-db {{messageDbVersion}}
