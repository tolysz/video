($scope, $mdSidenav, $rootScope, $log, maid, sections, title, wsLink) ->
  
  $scope.sections = sections
  $scope.maid = maid;
  $scope.title = title;

# if not the main menu
#  $rootScope.$on('$stateChangeSuccess', () ->
#       $log.debug "change"
#       $scope.closeLeft() )

  $scope.remote = wsLink.collection;
  
  $scope.player = "Test";

#  $scope.toggleLeft = () -> $mdSidenav('left') .toggle()
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
  $scope.toggleFullScreen = ->
      if !document.fullscreenElement and !document.mozFullScreenElement and !document.webkitFullscreenElement and !document.msFullscreenElement
        # current working methods
        if document.documentElement.requestFullscreen
          document.documentElement.requestFullscreen()
        else if document.documentElement.msRequestFullscreen
          document.documentElement.msRequestFullscreen()
        else if document.documentElement.mozRequestFullScreen
          document.documentElement.mozRequestFullScreen()
        else if document.documentElement.webkitRequestFullscreen
          document.documentElement.webkitRequestFullscreen Element.ALLOW_KEYBOARD_INPUT
      else
        if document.exitFullscreen
          document.exitFullscreen()
        else if document.msExitFullscreen
          document.msExitFullscreen()
        else if document.mozCancelFullScreen
          document.mozCancelFullScreen()
        else if document.webkitExitFullscreen
          document.webkitExitFullscreen()
      return