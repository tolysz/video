($scope, $mdSidenav, $log, maid, sections) ->
  
  $scope.sections = sections
  $scope.maid = maid;

  $scope.toggleLeft = () -> $mdSidenav('left') .toggle()
  $scope.openLeft   = () -> $mdSidenav('left') .open()
  $scope.closeLeft  = () -> $mdSidenav('left') .close()
  $scope.goHome     = () -> $log.debug "Yupi we are here"
  
  $scope.unselect   = (s) -> _.map( $scope.sections ,
       (s1) -> 
          if (s1 == s)
            s1.visible =  ! s1.visible
          else
            s1.visible = false
       )
