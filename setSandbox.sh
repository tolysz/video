#!/bin/bash

rm -rf .cabal-sandbox
cabal sandbox init

#cabal sandbox add-source /home/m/workHS/github/Haxl
cabal sandbox add-source /home/m/workHS/github/google-api
cabal sandbox add-source /home/m/workHS/github/typedquery
cabal sandbox add-source /home/m/workHS/github/postgresql-simple-typed
cabal sandbox add-source /home/m/workHS/github/http-client-extra
cabal sandbox add-source /home/m/workHS/github/http-client-extra-oauth2
cabal sandbox add-source /home/m/workHS/github/yesod-websockets-extra
cabal sandbox add-source /home/m/workHS/github/yesod-angular-ui
#cabal sandbox add-source /home/m/workHS/github/aeson
#cabal sandbox add-source /home/m/workHS/github/fb
#cabal sandbox add-source /home/m/workHS/github/keys

#cabal install -j typedquery -fdebug-typed-queries

cabal install -j --allow-newer aeson-0.8.0.2.tar.gz --constraint="typedquery +debug-typed-queries" -fdebug-typed-queries .
#aeson-0.10.0.0.tar.gz .

#cabal build
