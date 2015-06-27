[![Coverage Status](https://img.shields.io/coveralls/tolysz/video.png)](https://coveralls.io/r/tolysz/video)
[![Build Status](https://travis-ci.org/tolysz/video.png?branch=master)](https://travis-ci.org/tolysz/video)

Sample Webap
============

Sample website using Haskell/Yesod/AngularJS/Angular-Material
  Angular apps are generated on-the-fly depending on user and auths they have.
  Google APIs and so on... [Demo website running this code!](https://video.kio.sx/) uses facebook/google+/persona for auth
     This is still in development... all OAath2 might be insecure at times ( be sure, to logout via OAuth2 menu; might use secure session, to not to store anything)

Currently it only stores email, so your login will be recorded as user, and currently you will not see much;
But You could go to OAuth2 menu and login again to have access to your youtube channel (only you will have access to it)/ you can remove credential from the same manu.

OR just look at the code:
 Main is [Handler/Main.hs](https://github.com/tolysz/video/blob/master/Handler/Home.hs)
 Tempaltes are in [angular/](https://github.com/tolysz/video/blob/master/angular/)

INSTALL
=======

    cabal sandbox init
    cabal install yesod-angular-ui-0.1.0.0.tar.gz aeson-0.8.0.2.tar.gz  haxl-0.1.0.0.tar.gz .
    
* the aeson is a modified version... till patches be accepted
* yesod-angular-ui is a copy from github: [tolysz/yesod-angular-ui](https://github.com/tolysz/yesod-angular-ui)

Not in the repo stuff still in development
=========================

    bower init
    bower install bootstrap --save
    bower install angular   --save

    sudo npm -g install yuicompressor
    sudo npm -g install clean-css

It will generate the whole `static/` folder

this one will be replaced by y-bower

    npm install grunt-bower-concat --save-dev
    bower install angular-material#master
    sudo npm install -g vulcanize

still looking for a clean way to include it into the project


Running
======
You need to create a site admin, this will be the user who can create new sites, or even create new admins? (well let's not go this far)
one can run the command bellow for any user on a server which hosts your database

    mongo
    use somedatabase
    db.User.update({ident: "tolysz@gmail.com"},{$set:{siteAdmin:true}},{})

npm install --global postcss-cli autoprefixer
postcss --use autoprefixer *.css -d build/
