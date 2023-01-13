format:
 ormolu --mode inplace --ghc-opt=-XImportQualifiedPost message-db/src/**/*.hs
 ormolu --mode inplace --ghc-opt=-XImportQualifiedPost,-XOverloadedRecordDot message-db/test/**/*.hs

build:
 cabal build all
