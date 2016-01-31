#!/bin/bash

PWD=$(pwd)

cd server

stack install

APP=video
DIR=$(mktemp -d)
FN="${APP}${1}"

echo  ${DIR}
#mkdir -p dist/build/video
mkdir -p ${DIR}/config
cp config/keter${1}.yml ${DIR}/config/keter.yml
cp config/client_session_key.aes ${DIR}/config/
cp -r static ${DIR}
cp -f ${HOME}/.local/bin/${APP} ${DIR}/

cd ${DIR}
tar czfv - ${APP} config static > ${FN}.keter
scp ${FN}.keter m@s1.tolysz.org:/opt/keter/incoming

cd ${PWD}
rm -rf ${DIR}

#/app/bin/yesod keter -n
