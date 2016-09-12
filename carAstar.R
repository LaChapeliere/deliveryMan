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

computeAStarScore <- function(traffic, parent, child, goal) {
  #First computes the heuristic (manhattan distance) of the child
  #parent and child are lists
  childCoord = c(child[['x']], child[['y']])
  print("TEST")
  heuri = manhattanDistance(childCoord, goal)
  
  #Then finds the cost ot the movement from the parent to the child
  cost = 0
  if (parent[['x']] == child[['x']]) {
    #vertical movement
    cost = traffic['vroads'][parent[['x']], min(parent[['y']], child[['y']])]
  }
  else if (parent[['y']] == child[['y']]) {
    #horizontal movement
    cost = traffic['hroads'][min(parent[['x']], child[['x']]), parent[['y']]]
  }
  else {
    print("IMPOSSIBLEEEEEE")
  }
  
  return(heuri + cost)
}

addNeighboursToFrontier <- function(traffic, current, history) {
  #For each possible neighbour of current
  for (x in c((current[['x']] - 1),(current[['x']] + 1))) {
    for (y in c((current[['y']] - 1),(current[['y']] + 1))) {
      # -check if it exists
      if (x < 1 || x > 10 || y < 1 || y > 10) {
        #If doesn't exist, next
        next()
      }
      
      path = current[['path']]
      path[length(path)+1] <- c(x = current[['x']])
      path[length(path)+1] <- c(y = current[['y']])
      # -check if it has been visited
      if (history[x,y][['score']] == 0) {
        #If the node has never been visited, add it to the frontier
        neighbour = list(x = x, y = y, path = path, score = 0)
        neighbour[['score']] = computeAStarScore(traffic, current, neighbour, goal)
        history[x, y] = neighbour
      }
      else if (history[x,y][['score']] == -1) {
        #If the node has already been explored, go to the next neighbour
        next()
      }
      else {
        #If the node is already in the frontier,
        #only replace the current occurence of the node if
        #the new score is better
        currentScore = history[x,y][['score']]
        neighbour = list(x = x, y = y, path = path, score = 0)
        newScore = computeAStarScore(traffic, current, neighbour, goal)
        if (newScore > currentScore) {
          history[x,y][['score']] = newScore
        }
      }
    }
  }
  
  return(history)
}

aStarMain <- function(traffic, car, goal) {
  #We initialize current with the car position
  current = list(x = 0, y = 0, path = c(), score = 0)
  current[['x']] = car[['x']]
  current[['y']] = car[['y']]
  currentCoord = c(current[['x']], current[['y']])
  current[['score']] = manhattanDistance(currentCoord, goal)
  
  #Matrix representing the grid, every node is a list with 'x', 'y', 'path', and 'score' members
  #score has value 0 if not visited, -1 if explored
  #If the node is in the frontier, its value is its corresponding score
  history = matrix(nrow = 10, ncol = 10)
  for (i in 1:nrow(history)) {
    for (j in 1:ncol(history)) {
      history[i,j] = list(x = i, y = j, path = c(), score = 0)
    }
  }
  
  #While we have not reached the goal
  while(current[['x']] != goal['x'] || current[['y']] != goal['y']) {
    #We add unvisited neighbours of current to the frontier
    history = addNeighboursToFrontier(traffic, current, history)
    
    #We set the current node score to -1
    history[current[['x']], current[['y']]][['score']] = -1
    
    #We look for the lowest score to choose the next node to explore
    bestScore = 2000
    bestNode = list()
    for (i in 1:nrow(history)) {
      for (j in 1:ncol(history)) {
        if (history[i,j][['score']] > 0) {
          #The node is in the frontier
          if (history[i,j][['score']] < bestScore) {
            bestScore = history[i,j][['score']]
            bestNode = history[i,j]
          }
        }
      }
    }
    
    #Set current to the best node in the frontier
    current = bestNode
  }
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
  # goal = getNextPackageOrDelivery(car, deliveries)
  # car['nextMove'] = nextMoveVerticalThenHorizontal(car, goal)
  
  #Closest package + AStar
  goal = getNextPackageOrDelivery(car, deliveries)
  car['nextMove'] = aStarMain(traffic, car, goal)
  
  return(car)
}