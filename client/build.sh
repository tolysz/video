#!/usr/bin/env bash

PATH=`stack path --bin-path  --stack-yaml /home/m/.stack/global/stack.yaml 2>/dev/null`
stack setup
stack build
exit

PP=`stack path --local-install-root`/bin/reflex-todomvc.jsexe
ccjs $PP/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node > ../server/static/v2.js

# open /bin/reflex-dom-stack-demo.jsexe/index.html
# https://github.com/reinh/reflex-dom-stack-demo
# servius -d `stack path --local-install-root`/bin/reflex-dom-stack-demo.jsexe/
