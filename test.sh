#!/bin/bash

clu app.min ../bower_components/underscore/underscore.js ../bower_components/angular/angular.js ../bower_components/SHA-1/sha1.js ../bower_components/jquery/dist/jquery.js ../bower_components/waypoints/waypoints.js ../bower_components/angulartics/src/angulartics.js ../bower_components/angulartics/src/angulartics-adobe.js ../bower_components/angulartics/src/angulartics-chartbeat.js ../bower_components/angulartics/src/angulartics-cnzz.js ../bower_components/angulartics/src/angulartics-flurry.js ../bower_components/angulartics/src/angulartics-ga-cordova.js ../bower_components/angulartics/src/angulartics-ga.js ../bower_components/angulartics/src/angulartics-gtm.js ../bower_components/angulartics/src/angulartics-kissmetrics.js ../bower_components/angulartics/src/angulartics-mixpanel.js ../bower_components/angulartics/src/angulartics-piwik.js ../bower_components/angulartics/src/angulartics-scroll.js ../bower_components/angulartics/src/angulartics-segmentio.js ../bower_components/angulartics/src/angulartics-splunk.js ../bower_components/angulartics/src/angulartics-woopra.js ../bower_components/angulartics/src/angulartics-marketo.js ../bower_components/angulartics/src/angulartics-intercom.js ../bower_components/angular-resource/angular-resource.js ../bower_components/angular-sanitize/angular-sanitize.js ../bower_components/angular-aria/angular-aria.js ../bower_components/angular-animate/angular-animate.js ../bower_components/angular-material/angular-material.js ../bower_components/angular-websocket/angular-websocket.js ../bower_components/angular-ui-router/release/angular-ui-router.js ../bower_components/angular-cookies/angular-cookies.js
cp -r ../bower_components/angular-i18n .

mkdir -p `dirname bootstrap-fonts.cssx` && cp -r ../bower_components/web-fonts-collection/bootstrap-fonts.css bootstrap-fonts.css
mkdir -p `dirname font-awesome.cssx` && cp -r ../bower_components/web-fonts-collection/font-awesome.css font-awesome.css
mkdir -p `dirname font-tools.cssx` && cp -r ../bower_components/web-fonts-collection/font-tools.css font-tools.css
mkdir -p `dirname fonts/x` && cp -r ../bower_components/web-fonts-collection/fonts/* fonts/
mkdir -p `dirname angular-material.cssx` && cp -r ../bower_components/angular-material/angular-material.css angular-material.css

echo -e " @import url('bootstrap-fonts.css');\n @import url('font-awesome.css');\n @import url('font-tools.css');\n @import url('angular-material.css');\n" > app.css

cd ..
