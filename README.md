Sample Webap
============

Sample website using Haskell/Yesod/Angular/Polymer/WebComponents...
  Google APIs and so on... [Demo website running this code!](https://video.kio.sx/) uses persona for auth
     This is still in development... all OAath2 might be insecure at times (might use secure session, to not to store anything)

Currently it only stores email, all other stuff will be per user.

INSTALL
=======

    cabal sandbox init
    cabal install yesod-angular-ui-0.1.0.0.tar.gz aeson-0.8.0.2.tar.gz .
    
* the aeson is a modified version... till patches be accepted
* yesod-angular-ui is a copy from github: tolysz/yesod-angular-ui

Not in the repo stuff still in development
=========================

    bower init
    bower install bootstrap --save
    bower install angular   --save

It will generate the whole `static/` folder

sudo:

   this one will be replaced by y-bower
    npm install grunt-bower-concat --save-dev

    bower install angular-material#master
    sudo npm install -g vulcanize
still looking for a clean way to include it into the project


