#!/bin/bash

clu app.min ../bower_components/underscore/underscore.js ../bower_components/angular/angular.js ../bower_components/hammerjs/hammer.js ../bower_components/observe-js/src/observe.js ../bower_components/angular-sanitize/angular-sanitize.js ../bower_components/angular-aria/angular-aria.js ../bower_components/angular-animate/angular-animate.js ../bower_components/angular-material/angular-material.js ../bower_components/angular-ui-router/release/angular-ui-router.js
cp -r ../bower_components/URL .
cp -r ../bower_components/polymer-gestures .
cp -r ../bower_components/NodeBind .
cp -r ../bower_components/polymer-expressions .
cp -r ../bower_components/polymer .
cp -r ../bower_components/TemplateBinding .

mkdir -p `dirname css/bootstrap-fonts.cssx` && cp -r ../bower_components/web-fonts-collection/css/bootstrap-fonts.css css/bootstrap-fonts.css
mkdir -p `dirname css/font-awesome.cssx` && cp -r ../bower_components/web-fonts-collection/css/font-awesome.css css/font-awesome.css
mkdir -p `dirname fonts/x` && cp -r ../bower_components/web-fonts-collection/fonts/* fonts/
mkdir -p `dirname themes.cssx` && cp -r ../bower_components/angular-material-themes/themes.css themes.css
mkdir -p `dirname angular-material.cssx` && cp -r ../bower_components/angular-material/angular-material.css angular-material.css

echo -e " @import url('css/bootstrap-fonts.css');\n @import url('css/font-awesome.css');\n @import url('themes.css');\n @import url('angular-material.css');\n" > app.css

cd ..
