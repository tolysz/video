#!/usr/bin/env bash

PATH=`stack path --bin-path  --stack-yaml $HOME/.stack/global/stack.yaml 2>/dev/null`
echo SETUP
stack setup
echo BUILD
PP=`stack path --local-install-root`/bin/reflex-todomvc.jsexe
servius -d $PP -p 3301 &
WW=$1

stack build --file-watch
#echo CLOSURE-COMPILE
#ccjs $PP/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node > ../server/static/v2.js

kill $WW

# open /bin/reflex-dom-stack-demo.jsexe/index.html
# https://github.com/reinh/reflex-dom-stack-demo
# servius -d `stack path --local-install-root`/bin/reflex-dom-stack-demo.jsexe/
