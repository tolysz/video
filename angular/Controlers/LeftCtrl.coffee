($scope, $timeout, $mdSidenav, $log, maid) ->
  $scope.sections =
    [
      state : "demos"
      name:   "Demos"
      visible: true
      pages: [ { state: "demos.panel",     name: "Pannel",     icon: "fa columns" }
             , { state: "demos.button",    name: "Button",     icon: "fa barcode" }
             , { state: "demos.checkbox",  name: "Checkbox",   icon: "fa barcode" }
             , { state: "demos.content",   name: "Content",    icon: "fa barcode" }
             , { state: "demos.dialog",    name: "Dialog",     icon: "fa barcode" }
             , { state: "demos.slider",    name: "Slider",     icon: "fa barcode" }
             , { state: "demos.textfield", name: "Text Field", icon: "fa barcode" }
             , { state: "demos.youtube",   name: "Youtube",    icon: "fa youtube" }
             , { state: "demos.empty",     name: "Empty",      icon: "fa frown-o" }
             , { state: "demos.about",     name: "About",      icon: "fa info" }
             ]
    ]
  $scope.maid = maid;

  $scope.toggleLeft = () -> $mdSidenav('left') .toggle()
  $scope.openLeft   = () -> $mdSidenav('left') .open()
  $scope.closeLeft  = () -> $mdSidenav('left') .close()
  $scope.goHome     = () -> $log.debug "Yupi we are here"
