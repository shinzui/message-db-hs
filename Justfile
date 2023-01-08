format:
 ormolu --mode inplace --ghc-opt=-XImportQualifiedPost --debug message-db/src/**/*.hs

build:
 cabal build all
