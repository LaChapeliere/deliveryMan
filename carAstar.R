manhattanDistance <- function(A, B) {
  #Computes the manhattan distance between A and B
  #A and B each needs to be a list and have a 'x' and a 'y' coordinate members
  return(abs(A['x'] - B['x']) + abs(A['y'] - B['y']))
}

# <Waiting Optimization>
#
# nextMove <- function(car, history, goal, traffic) {
#   neighbourX = goal[['x']]
#   neighbourY = goal[['y']]
#   parentX = history[['parentXs']][neighbourX, neighbourY]
#   parentY = history[['parentYs']][neighbourX, neighbourY]
#   
#   if(goal['x'] == car['x'] && goal['y'] == car['y']){
#     nextMove = 5
#   }else{
#     while(parentX != car[['x']] || parentY != car[['y']]) {
#       neighbourX = parentX
#       neighbourY = parentY
#       parentX = history[['parentXs']][neighbourX, neighbourY]
#       parentY = history[['parentYs']][neighbourX, neighbourY]
#     }
#   }
#   
#   parentScore = history[['scores']][neighbourX, neighbourY]
#   neighbour = c(x=neighbourX, y=neighbourY)
#   carCoord = c(x = car[['x']],y = car[['y']])
#   waits = car$mem$wait
#   
#   #checks if you move away from the goal and if you've been waiting for more than a
#   #certain amount of turns for a better path.
#   if(waits<2 && manhattanDistance(neighbour,goal)>manhattanDistance(carCoord,goal)){
#     nextMove = 5
#     car$mem$wait = waits +1
#   }
#   #checks if the next move will be too costly and if you've been waiting for more than a
#   #certain amount of turns for the costs to go down.
#   else if(waits<2 && (mean(traffic[['vroads']]) + mean(traffic[['hroads']]) * 1.5) < parentScore){
#     nextMove = 5
#     car$mem$wait = waits + 1
#   }
#   else{
#     
#     if (neighbourY < car['y']) {
#       nextMove = 2
#     }
#     else if (neighbourY > car['y']) {
#       nextMove = 8
#     }
#     else {
#       if (neighbourX < car['x']) {
#         nextMove = 4
#       }
#       else if (neighbourX > car['x']) {
#         nextMove = 6
#       }
#       else {
#         print("Shouldn't be here")
#         nextMove = 5
#       }
#     }
#     car$mem$wait = 0
#   }
#   car['nextMove'] = nextMove
#   
#   return(car)
# }
#
# </Waiting Optimization>


getNextGoalOnBestPath <- function(car, deliveries) {
  #Check if the car has a package loaded
  #If yes, the goal is the package's delivery point
  goal = c(x = 0, y = 0, package = 0)
  
  if (car['load'] > 0) {
    #package loaded
    package = car[['load']]
    goal["x"] = deliveries[package,3]
    goal["y"] = deliveries[package,4]
    return(goal)
  }
  
  #If no
  #Brute force the possible paths to do all the pick-ups + deliveries
  #Choose the best one
  nbPackets = nrow(deliveries)
  leftPackets = c()
  nbLeftPackets = 0
  for (i in 1:nbPackets) {
    #List the packets that have not picked-up yet
    if (deliveries[i,5] == 0) {
      nbLeftPackets = nbLeftPackets + 1
      leftPackets[nbLeftPackets] <- i
    }
  }
  
  #Gets the cost from each delivery to each packet
  costsPacketToPacket = matrix(nrow = nbPackets, ncol = nbPackets)
  for (i in 1:nbPackets) {
    for (j in 1:nbPackets) {
      if (j >= i) {
        break
      }
      if (i == j) {
        costsPacketToPacket[i,j] = 100000000
      }
      else {
        distItoJ = manhattanDistance(c(x = deliveries[j,1], y = deliveries[j,2]), c(x = deliveries[i,3], y = deliveries[i,4]))
        costsPacketToPacket[i,j] = distItoJ
        distJtoI = manhattanDistance(c(x = deliveries[i,1], y = deliveries[i,2]), c(x = deliveries[j,3], y = deliveries[j,4]))
        costsPacketToPacket[j,i] = distJtoI
      }
    }
  }
  #No need to calculate the cost from each pick-up to its delivery because it is a constant
  
  #It's ok to do loops because there is no negative weight
  #Generates all permutations of pick-up order for packets that have not been picked-up yet
  permutations = iterpc(nbLeftPackets, nbLeftPackets, ordered = TRUE)
  best = 100000000
  bestFirst = -1
  for (index in 1:nrow(getall(permutations))) {
    permut = getall(permutations)[index,]
    carCoord = c(x = car[['x']],y = car[['y']])
    firstPickup = c(x = deliveries[leftPackets[permut[1]],1], y = deliveries[leftPackets[permut[1]],2])
    score = manhattanDistance(carCoord, firstPickup)
    for (i in 2:nbLeftPackets) {
      score = score + costsPacketToPacket[leftPackets[permut[i-1]],leftPackets[permut[i]]]
    }
    if (score < best) {
      best = score
      bestFirst = leftPackets[permut[i]]
    }
  }
  
  goal["x"] = deliveries[bestFirst,1]
  goal["y"] = deliveries[bestFirst,2]
  goal["package"] = bestFirst
  return(goal)
}

getNextPackageOrDeliveryClosestNextPickup <- function(car, deliveries, size) {
  #Check if the car has a package loaded
  #If yes, the goal is the package's delivery point
  #If not, the goal is the unpicked package with the smallest sum of
  #distance from the car to it
  #and distance from its delivery point and the unpicked package closest to this delivery point.
  
  #weight to give priority to one distance over the other
  #1 means only the first distance is taken into account
  #0 means only the second one is taken into account
  weight = 0.3
  
  goal = c(x = 0, y = 0)
  
  if (car['load'] > 0) {
    #package loaded
    package = car[['load']]
    goal["x"] = deliveries[package,3]
    goal["y"] = deliveries[package,4]
  }
  else {
    #For each unpicked package, compute the distance between it and the car
    closestDistance = size * 4
    closest = 0
    carCoord = c(x = car[['x']],y = car[['y']])
    for (i in 1:nrow(deliveries)) {
      if (deliveries[i,5] == 0) {
        #If the package has not been picked up yet
        pickup = c(x = deliveries[i,1], y = deliveries[i,2])
        distance = manhattanDistance(carCoord, pickup)
        closestDistance2 = size * 2
        #Look for the unpicked package closest to the current package delivery point
        for (j in 1:nrow(deliveries)) {
          if (i != j && deliveries[j,5] == 0) {
            delivery = c(x = deliveries[i,3], y = deliveries[i,4])
            nextPickup = c(x = deliveries[j,1], y = deliveries[j,2])
            distance2 = manhattanDistance(delivery, nextPickup)
            if (distance2 < closestDistance2) {
              closestDistance2 = distance2
            }
          }
        }
        
        sum = weight * distance + (1 - weight) * closestDistance2
        if (sum < closestDistance) {
          closestDistance = sum
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



nextMove <- function(car, history, goal) {
  neighbourX = goal['x']
  neighbourY = goal['y']
  parentX = history[['parentXs']][neighbourX, neighbourY]
  parentY = history[['parentYs']][neighbourX, neighbourY]
  
  if(goal['x'] == car['x'] && goal['y'] == car['y']){
    nextMove = 5
  }else(
    while(parentX != car[['x']] || parentY != car[['y']]) {
      neighbourX = parentX
      neighbourY = parentY
      parentX = history[['parentXs']][neighbourX, neighbourY]
      parentY = history[['parentYs']][neighbourX, neighbourY]
    }
  )
  
  if (neighbourY < car['y']) {
    nextMove = 2
  }
  else if (neighbourY > car['y']) {
    nextMove = 8
  }
  else {
    if (neighbourX < car['x']) {
      nextMove = 4
    }
    else if (neighbourX > car['x']) {
      nextMove = 6
    }
    else {
      nextMove = 5
    }
  }
  
  return(nextMove)
}

getNextPackageOrDelivery <- function(car, deliveries, size) {
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
    closestDistance = size * 2
    closest = 0
    for (i in 1:nrow(deliveries)) {
      if (deliveries[i,5] == 0) {
        #If the package has not been picked up yet
        carCoord = c(x = car[['x']],y = car[['y']])
        pickup = c(x = deliveries[i,1], y = deliveries[i,2])
        distance = manhattanDistance(carCoord, pickup)
        if (distance <= closestDistance) {
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
  heuri = manhattanDistance(child, goal)
  #Then finds the cost ot the movement from the parent to the child
  cost = 0
  if (parent['x'] == child['x']) {
    #vertical movement
    cost = traffic[['vroads']][min(parent['y'], child['y']), parent['x']]
  }
  else if (parent['y'] == child['y']) {
    #horizontal movement
    cost = traffic[['hroads']][parent['y'], min(parent['x'], child['x'])]
  }
  else {
    print("IMPOSSIBLEEEEEE")
  }
  
  return(heuri + cost)
}

addNeighboursToFrontier <- function(traffic, current, history, goal) {
  size = ncol(traffic[['vroads']])
  
  #For each possible neighbour of current
  for (x in (current['x'] - 1):(current['x'] + 1)) {
    for (y in (current['y'] - 1):(current['y'] + 1)) {
      # -check it's not the node itself
      if (x == current['x'] && y == current['y']) {
        next()
      }
      
      # -check it's a neighbour (and not a diagonal neighbour)
      if (x != current['x'] && y != current['y']) {
        next()
      }
      
      # -check if it exists
      if (x < 1 || x > size || y < 1 || y > size) {
        #If doesn't exist, next
        next()
      }
      
      # -check if it has been visited
      if (history[['scores']][x,y] == 0) {
        #If the node has never been visited, add it to the frontier
        neighbour = c(x = x, y = y)
        history[['scores']][x,y] = computeAStarScore(traffic, current, neighbour, goal)
        history[['parentXs']][x,y] = current['x']
        history[['parentYs']][x,y] = current['y']
      }
      else if (history[['scores']][x,y] == -1) {
        #If the node has already been explored, go to the next neighbour
        next()
      }
      else {
        #If the node is already in the frontier,
        #only replace the current occurence of the node if
        #the new score is better
        currentScore = history[['scores']][x,y]
        neighbour = c(x = x, y = y)
        newScore = computeAStarScore(traffic, current, neighbour, goal)
        if (newScore > currentScore) {
          history[['scores']][x,y] = newScore
          history[['parentXs']][x,y] = current['x']
          history[['parentYs']][x,y] = current['y']
        }
      }
    }
  }
  
  return(history)
}

aStarMain <- function(traffic, car, goal) {
  size = ncol(traffic[['vroads']])
  
  #We initialize current with the car position
  current = c(x = 0, y = 0)
  current['x'] = car[['x']]
  current['y'] = car[['y']]
  current['score'] = manhattanDistance(current, goal)
  
  #Matrices representing the grid, score matrix, parentX matrix and parentY matrix
  #score has value 0 if not visited, -1 if explored
  #If the node is in the frontier, its value is its corresponding score
  scores = matrix(nrow = size, ncol = size)
  parentXs = matrix(nrow = size, ncol = size)
  parentYs = matrix(nrow = size, ncol = size)
  
  for (i in 1:nrow(scores)) {
    for (j in 1:ncol(scores)) {
      scores[i,j] = 0
      parentXs[i,j] = 0
      parentYs[i,j] = 0
    }
  }
  history = list(scores = scores, parentXs = parentXs, parentYs = parentYs)
  
  #While we have not reached the goal
  while(current['x'] != goal['x'] || current['y'] != goal['y']) {
    
    #We add unvisited neighbours of current to the frontier
    history = addNeighboursToFrontier(traffic, current, history, goal)
    
    #We set the current node score to -1
    history[['scores']][current['x'], current['y']] = -1
    
    #We look for the lowest score to choose the next node to explore
    bestScore = size * size * 1000
    bestNode = c()
    for (i in 1:nrow(history[['scores']])) {
      for (j in 1:ncol(history[['scores']])) {
        if (history[['scores']][i,j] > 0) {
          #The node is in the frontier
          if (history[['scores']][i,j] < bestScore) {
            bestScore = history[['scores']][i,j]
            bestNode = c(x = i, y = j, parentX = history[['parentXs']][i,j], parentY = history[['parentYs']][i,j], score = bestScore)
          }
        }
      }
    }
    #Set current to the best node in the frontier
    current = bestNode
  }
  
  # <Waiting Optimizations>
  #return(nextMove(car,history,goal, traffic))
  # </Waiting Optimizations>
  
  return(nextMove(car, history, goal))
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
  
  # <Waiting Optimizations>
  #
  # if(length(car$mem)==0){
  #   car$mem$wait = 0
  # }
  # 
  # #Closest package + AStar
  # goal = getNextPackageOrDelivery(car, deliveries, ncol(traffic[['vroads']]))
  # car = aStarMain(traffic, car, goal)
  # return(car)
  #
  # </Waiting Optimizations>
  
  
  # <Best Path>
  # 
  # if (length(car$mem) == 0) {
  #   car$mem$lastGoal = 0
  #   car$mem$secondToLastGoal = 0
  # }
  # if (nrow(deliveries) <= 5) {
  #   goal = getNextGoalOnBestPath(car, deliveries)
  #   if (length(car$mem) == 0) {
  #     car$mem$lastGoal = 0
  #     car$mem$secondToLastGoal = 0
  #   }
  #   lastGoal = car$mem$lastGoal
  #   secondToLastGoal = car$mem$secondToLastGoal
  #   #If stuck in a loop, keep the same goal as before
  #   if (lastGoal != secondToLastGoal && secondToLastGoal == goal["package"]) {
  #     goal["x"] = deliveries[lastGoal,1]
  #     goal["y"] = deliveries[lastGoal,2]
  #     goal["package"] = lastGoal
  #   }
  #   car$mem$secondToLastGoal = lastGoal
  #   car$mem$lastGoal = goal["package"]
  # }
  # else {
  #   goal = getNextPackageOrDelivery(car, deliveries, ncol(traffic[['vroads']]))
  # }
  # car['nextMove'] = aStarMain(traffic, car, goal)
  # 
  # return(car)
  #
  # </Best Path>
  
  
  
  # <Closest Next Pickup>
  # 
  # if (length(car$mem) == 0) {
  #   car$mem$lastGoal = 0
  #   car$mem$secondToLastGoal = 0
  # }
  # 
  # goal = getNextPackageOrDeliveryClosestNextPickup(car, deliveries, ncol(traffic[['vroads']]))
  # car['nextMove'] = aStarMain(traffic, car, goal)
  #
  #return(car)
  #
  # </Closest Next Pickup>
  
  
  
  #Closest package + AStar
  goal = getNextPackageOrDelivery(car, deliveries, ncol(traffic[['vroads']]))
  car['nextMove'] = aStarMain(traffic, car, goal)
  
  return(car)
}