#!/bin/sh

wget --no-check-certificate --no-cookies --header "Cookie: oraclelicense=accept-securebackup-cookie" http://download.oracle.com/otn-pub/java/jdk/8u66-b17/jre-8u66-linux-x64.tar.gz
tar -xvf jre-8u66-linux-x64.tar.gz
sudo mkdir -p /opt/jre
sudo mv jre-8u66-linux-x64/* /opt/jre/
sudo update-alternatives --install "/usr/bin/java" "java" "/opt/jre/bin/java" 1
