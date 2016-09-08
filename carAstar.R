manhattanDistance <- function(A, B) {
  #Computes the manhattan distance between A and B
  #A and B each needs to be a list and have a 'x' and a 'y' coordinate members
  return(abs(A['x'] - B['x']) + abs(A['y'] - B['y']))
}

nextMoveVerticalThenHorizontal <- function(car, goal) {
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
  
  return(nextMove)
}

getNextPackageOrDelivery <- function(car, deliveries) {
  #Check if the car has a package loaded
  #If yes, the goal is the package's delivery point
  #If not, the goal is the closest unpicked package
  
  goal = c(x = 0, y = 0)
  
  if (car['load'] > 0) {
    #package loaded
    package = car[['load']]
    goal["x"] = deliveries[package,3]
    goal["y"] = deliveries[package,4]
  }
  else {
    #Look for the closest unpicked package in the deliveries list
    closestDistance = 20
    closest = 0
    for (i in 1:nrow(deliveries)) {
      if (deliveries[i,5] == 0) {
        #If the package has not been picked up yet
        carCoord = c(x = car[['x']],y = car[['y']])
        pickup = c(x = deliveries[i,1], y = deliveries[i,2])
        distance = manhattanDistance(carCoord, pickup)
        if (distance < closestDistance) {
          closestDistance = distance
          closest = i
        }
      }
    }
    goal["x"] = deliveries[closest,1]
    goal["y"] = deliveries[closest,2]
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

  #Closest package, first vertical than horizontal
  #Choose next package or delivery
  goal = getNextPackageOrDelivery(car, deliveries)
  car['nextMove'] = nextMoveVerticalThenHorizontal(car, goal)
  
  return(car)
}