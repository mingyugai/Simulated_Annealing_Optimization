#compute two citis distance
city.distance <- function(location){
  distance.matrix <- matrix(0, ncol = 100, nrow = 100) 
  for (i in seq(1:ncol(location))){
    for (j in seq(1:ncol(location))){
    distance.matrix[i, j] = sqrt(sum((location[, i]-location[, j])^2))
    } 
  }
  return(distance.matrix) 
} 

#comput all the citis distance
distance <- function(cities, matrix){ 
  d <- 0
  for (i in seq(1:(length(cities)-1))){
    d = d + matrix[cities[i], cities[i+1]]
  }
  d = d + matrix[cities[length(cities)], cities[1]] 
  return(d)
}
##ompute difference of distances when we change the route
diff.distance <- function(city.1, city.2, cities, matrix){ 
  new.cities <- cities
  if (city.1 == 1){
    city.1.pre <- 100 
    }else{
    city.1.pre <- city.1 - 1 
    }
  if (city.2 == 100){ 
    city.2.post <- 1
  }else{
    city.2.post <- city.2 + 1
  }
  if((city.2 - city.1) == 99){
    diff <- 0
  }else{
    prev.distance <- matrix[cities[city.1.pre],cities[city.1]] + matrix[cities[city.2], cities[city.2.post]]
    new.distance <- matrix[cities[city.1.pre], cities[city.2]] + matrix[cities[city.1], cities[city.2.post]]
    diff <- (new.distance - prev.distance) 
    }
  for (swap in c(city.1 : city.2)) {
  new.cities[swap] <- cities[city.2 + city.1 - swap]
  }
  return(list(new.cities, diff))
}
##apply simulated annealing method
simulatedannealing <- function(cities, initial.temperature, cooling.rate, threshold, matrix){
  temperature <- initial.temperature 
  iterations <- 1
  temperature.iterations <- 0
  last.distance <- distance(cities, matrix) 
  while (iterations <= threshold){
    ## random swap two edges
    swap <- sample(cities, size = 2, replace = FALSE) 
    city.1 <- min(swap)
    city.2 <- max(swap)
    ## compute the difference of the two cities
    diff <- diff.distance(city.1, city.2, cities, matrix) 
    if(temperature.iterations >= 1000){
      ## each 1000 iterations change the temperature once
      temperature <- cooling.rate*temperature
      temperature.iterations <- 0 }
    if (runif(1) < exp(-diff[[2]]/temperature)){ 
      cities <- diff[[1]]
      new.distance <- distance(cities, matrix)
      if (round(new.distance, 2) != round(last.distance, 2)) { 
        last.distance <- new.distance
      }
    }
    iterations <- iterations + 1
    temperature.iterations <- temperature.iterations + 1
  }
  return(list(cities,new.distance))
}
##compute the result
cities <- seq(1, 100, by = 1)
city1<-simulatedannealing(cities,1,0.99,1000000000, distance.matrix1)
city2<-simulatedannealing(cities,1,0.999,1000000000, distance.matrix2)
new.location11 <- location1[, city1[[1]]]
new.location22 <- location2[, city2[[1]]]
plotcities <- function(location){
  plot(location[1, ],location[2, ]) 
  lines(location[1, ],location[2, ]) 
  lines(location[1, c(1,100)],location[2,c(1,100) ])
}
##plot the graph
plotcities(new.location11) 
plotcities(new.location22)