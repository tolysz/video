#!/usr/bin/env bash

PATH=`stack path --bin-path  --stack-yaml /home/m/.stack/global/stack.yaml 2>/dev/null`
#stack exec ghcjs-boot -- --clean
# PATH=`stack path --bin-path  --stack-yaml /home/m/.stack/global/stack.yaml 2>/dev/null`

#exit
PP=`stack path --local-install-root`/bin/reflex-todomvc.jsexe


rm -f ../server/static/v2.js
#ln -s $PP/all.js ../server/static/v2.js

echo "Build"
# filewatcher '**/all.js' 'ccjs $FILENAME --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node > ../server/static/v2.js' &
#filewatcher -l '.stack-work/install/**/all.js' 'ccjs $FILENAME --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node > ../server/static/v2.js ; touch ../server/templates/v2.hamlet' &
#FW=$!
#stack build --file-watch

#kill $FW

ccjs $PP/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node > ../server/static/v2.js

# open /bin/reflex-dom-stack-demo.jsexe/index.html
# https://github.com/reinh/reflex-dom-stack-demo
# servius -d `stack path --local-install-root`/bin/reflex-dom-stack-demo.jsexe/
