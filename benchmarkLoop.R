#Function that repeats the runDeliveryMan function with the provided arguments.
#Returns a vector with the number of turns for each try.
benchmarkLoop <- function(carReady, dim, turns, doPlot, pause, del, numberOfRuns) {
  results = integer(numberOfRuns)
  for (i in 1:numberOfRuns) {
    results[i] = runDeliveryMan(carReady = carReady, dim = dim, turns = turns, doPlot = doPlot, pause = pause, del = del)
  }
  print("Results")
  print(results)
  print("Mean")
  print(mean(results))
  print("Standard Deviation")
  print(sd(results))
  return(results)
}