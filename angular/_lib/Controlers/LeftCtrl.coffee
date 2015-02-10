($scope, $mdSidenav, $log, maid, sections, wsLink) ->
  
  $scope.sections = sections
  $scope.maid = maid;
  
  $scope.remote = wsLink.collection;
  
  $scope.player = "Test";

  $scope.toggleLeft = () -> $mdSidenav('left') .toggle()
  $scope.openLeft   = () -> $mdSidenav('left') .open()
  $scope.closeLeft  = () -> $mdSidenav('left') .close()
  $scope.goHome     = () -> $log.debug "Yupi we are here"
  
  # make it double click to select topic/expandable state
  $scope.unselect   = (s) -> _.map( $scope.sections ,
       (s1) -> 
          if (s1 == s)
            s1.visible =  ! s1.visible
            wsLink.get(s)
          else
            s1.visible = false
       )
