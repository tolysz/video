#!/bin/bash

clu app.min ../bower_components/webcomponentsjs/webcomponents.js ../bower_components/underscore/underscore.js ../bower_components/angular/angular.js ../bower_components/angular-resource/angular-resource.js ../bower_components/angular-sanitize/angular-sanitize.js ../bower_components/angular-aria/angular-aria.js ../bower_components/angular-animate/angular-animate.js ../bower_components/angular-material/angular-material.js ../bower_components/angular-websocket/angular-websocket.js ../bower_components/angular-ui-router/release/angular-ui-router.js ../bower_components/polymer/polymer.js ../bower_components/angular-cookies/angular-cookies.js

mkdir -p `dirname css/bootstrap-fonts.cssx` && cp -r ../bower_components/web-fonts-collection/css/bootstrap-fonts.css css/bootstrap-fonts.css
mkdir -p `dirname css/font-awesome.cssx` && cp -r ../bower_components/web-fonts-collection/css/font-awesome.css css/font-awesome.css
mkdir -p `dirname fonts/x` && cp -r ../bower_components/web-fonts-collection/fonts/* fonts/
mkdir -p `dirname angular-material.cssx` && cp -r ../bower_components/angular-material/angular-material.css angular-material.css

echo -e " @import url('css/bootstrap-fonts.css');\n @import url('css/font-awesome.css');\n @import url('angular-material.css');\n" > app.css

cd ..
