getNextPackageOrDelivery <- function(car, deliveries) {
  #Check if the car has a package loaded
  #If yes, the goal is the package's delivery point
  #If not, the goal is the next unpicked package in the list
  
  goal = c(x = 0, y = 0)
  
  if (car['load'] > 0) {
    #package loaded
    package = car[['load']]
    print(package)
    print(deliveries)
    goal["x"] = deliveries[package,3]
    goal["y"] = deliveries[package,4]
  }
  else {
    #Look for the next unpicked package in the deliveries list
    for (i in 1:length(deliveries)) {
      if (deliveries[i,5] == 0) {
        #If the package has not been picked up yet
        goal["x"] = deliveries[i,1]
        goal["y"] = deliveries[i,2]
        break
      }
    }
  }
  
  #return a list of coordinates to the goal 
  return(goal)
}





carAstar <- function(traffic, car, deliveries) {
  #traffic: list of two matrices giving the traffic conditions.
  #First one is 'hroads' and gives the traffic conditions for horizontal roads
  #Second on is 'vroads' and gives the traffic conditions for vertical roads
  #car: list of x and y coordinates (names 'x' and 'y')
  #and the package the car is carrying ('load') (0 if no package is carried)
  #and a list 'mem' to store info from one turn to another
  #and a field 'nextMove' to specify the move who want to make
  #(2 down, 4 left, 6 right, 8 up, 5 stay still)
  #deliveries: matrix with info on the packages to deliver,
  #one row per package,
  #first two columns are the x and y of the package,
  #next two columns are the x and y of the delivery point,
  #last column is the package status
  #(0 is not picked up, 1 is picked up but not delivered, 2 is delivered)
  
  #Function should return the car object with the nextMove specified
  
  possibleOutputs = c(2, 4, 6, 8, 5)
  
  #RANDOM WALK
  # possibleOutputs
  # car['nextMove'] = sample(possibleOutputs, 1)
  # car['nextMove']
  
  #Next package, first vertical than horizontal
  #Choose next package or delivery
  goal = getNextPackageOrDelivery(car, deliveries)
  if (goal['y'] < car['y']) {
    nextMove = 2
  }
  else if (goal['y'] > car['y']) {
    nextMove = 8
  }
  else {
    if (goal['x'] < car['x']) {
      nextMove = 4
    }
    else if (goal['x'] > car['x']) {
      nextMove = 6
    }
    else {
      nextMove = 5
    }
  }
  car['nextMove'] = nextMove
  
  
  return(car)
}