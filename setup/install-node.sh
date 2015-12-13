#!/bin/sh

wget https://nodejs.org/dist/v5.2.0/node-v5.2.0-linux-x64.tar.gz
tar -xvf node-v5.2.0-linux-x64.tar.gz
sudo rm -R /opt/node
sudo mkdir -p /opt/node

sudo mv node-v5.2.0-linux-x64/* /opt/node/
sudo update-alternatives --install "/usr/bin/node" "node" "/opt/node/bin/node" 1
sudo update-alternatives --install "/usr/bin/npm" "npm" "/opt/node/bin/npm" 1
