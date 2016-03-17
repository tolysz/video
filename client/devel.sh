#!/usr/bin/env bash

PATH=`stack path --bin-path  --stack-yaml /home/m/.stack/global/stack.yaml 2>/dev/null`
stack setup
stack build

exit 0
PP=`stack path --local-install-root`/bin/reflex-todomvc.jsexe

rm -f ../server/static/v2.js
filewatcher -l '.stack-work/install/**/all.js' 'ccjs $FILENAME --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node > ../server/static/v2.js ; touch ../server/templates/v2.hamlet' &
FW=$!
stack build --file-watch

kill $FW
