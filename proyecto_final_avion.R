max_weight = 500
 values <-   c(55, 70,
              31, 32,
              43, 36,
              23, 65,
              48, 49,
              40, 62,
              53, 25,
              54, 47,
              59, 78,
              77, 65,
              49, 23,
              59, 45,
              53, 43,
              28, 44,
              71, 71,
              36, 67,
              62, 48,
              30, 36,
              48, 76,
              42, 2)
weights_pay <- matrix(values,ncol = 2, byrow = TRUE) 
colnames(weights_pay)<-c('weight','pay')
head(weights_pay)

chromosome <-function(weights){
  weight   <- 0
  pool     <- weights
  pool_idx <- 1:length(pool)
  selected <- c()
  for(i in 1:length(pool)){
    value_idx <- sample(pool_idx,1)
    pool_idx  <- setdiff(pool_idx,value_idx)
    weight    <- weight+pool[value_idx]
    if(weight > max_weight) break
    selected  <- c(selected,value_idx)
  }
  out <- rep(0,length(pool) )
  out[selected] <- 1
  return(out)
}


chromosome(weights_pay[,2])

population <- t(sapply(1:500,function(x,w){chromosome(w)},w=weights_pay[,2] ))
population <- cbind(population,population %*% weights_pay[,1])


##### crossover half weight 
crossover <- function(child){
  p1<- population[sample(1:500,1),-11]
  p2 <- population[sample(1:500,1),-11]
  (rcut <- which(cumsum(p1*weights_pay[,2])>max_weight/2)[1])
  if(!is.na(rcut)){
    (lcut <- rcut-1)
    proto_child <- c(p1[1:lcut],p2[rcut:20])
  } else {
    (rcut <- which(cumsum(p2*weights_pay[,2])>max_weight/2)[1])
    (lcut <- rcut-1)
    proto_child <- c(p2[1:lcut],p1[rcut:20])
  }
  remove_weight <- which(cumsum(proto_child*weights_pay[,1])>500)
  proto_child[remove_weight] <- 0
  child <- proto_child
  return(child)
}

#Mutate 
mutar <- function(child){
  child <- child[2:(length(child)-1)] 
  index<-sample(1:length(child),2)
  i <- child[index[1]]
  child[index[1]] <- child[index[2]]
  child[index[2]] <- i
  child <- c(1,child,1)
  return(child)
}

N = 20
new_pop <- matrix(rep(0,N=1),ncol = N+1)
for (i in 1:500) {
  parents <- sample(1:nrow(population),size = 2)
  padres <- population[parents,1:(N+1)]
  hijo<-crossover(padres)
  if(runif(1)<0.01){
    hijo<-mutar(hijo)
  }
  new_pop <- rbind(new_pop,hijo)
}
population <- new_pop[-1,-1]
population <- cbind(population,population %*% weights_pay[,1])

generate_cities <- function(cities =5){
  pos_y <- sample(length(weights_pay[,2]), size = cities , replace=TRUE)
  weight <- c()
  pay<- c()
  for(i in 1:length(pos_y)){
    weight[i]<-weights_pay[pos_y[i],1]
    pay[i]<- weights_pay[pos_y[i],2]
  }
  out <- data.frame(city = 1:cities,weight,pay)
  return(out)
}

distance_route <- function(route,distance){
  sum_distance<-0
  for(i in 1:length(route[-1])){
    out <- distance[ route[i], route[i+1]  ]
    sum_distance <- sum_distance + out
  }
  return(sum_distance)
}
sample_route <- function(initial_node = 1, nodes = 4){
  nodes <- 1:nodes
  sample_nodes <- nodes[-initial_node]
  attach_nodes <- sample(sample_nodes)
  out <- c(initial_node,attach_nodes,initial_node)
  return(out)
}
initial_route <- function(n=200,cities_dist){
  dist<-data.frame()
  cities <- nrow(cities_dist)
  for(i in 1:n){
    ruta <- sample_route(1,nrow(cities_dist))
    dist<-rbind( dist, c(ruta, distance_route(ruta,cities_dist) )  )
  }
  names(dist)[ncol(dist)]<-"distance"
  x <- t(dist[dist$distance==min(dist$distance), ])[,1]
  x <- as.integer(x[1:(cities+1) ])
  return(x)
} 

rnd_neighbord <- function(vec){
  n <- length(vec)-1
  change <- sample(2:n, size = 2, replace = FALSE)
  temp <- vec[change[1]]
  vec[change[1]] <- vec[change[2]]
  vec[change[2]] <- temp
  return(vec)
} 
anneal <- function(cities=5,N=300, temp=10,   alpha = 0.9){
  repeat{
    temp_min = 0.001
    cities_space <- generate_cities(cities)
    cities_dist <- as.matrix(dist(cities_space[,2:3]))
    ruta <- initial_route(N,cities_dist)
    distancia <- distance_route(ruta,cities_dist)
    while(temp > temp_min){
      i <- 1
      while(i <= 100){
        new_route <- rnd_neighbord(ruta)
        new_distancia <- distance_route(new_route,cities_dist) 
        acceptance_probability <- exp( (distancia-new_distancia)/temp)
        if(acceptance_probability < runif(1,0,1) ){
          ruta <- new_route
          distancia <- new_distancia
        } else if(new_distancia<distancia) {
          distancia <- new_distancia
          ruta <- new_route
        }
        i <- i+1
      }
      temp <- temp*alpha
    }
    if(sum(cities_space[,2])<=500){
      print(sum(cities_space[,3]))
      plot(cities_space[,2:3])
      x<- ruta 
      polygon(cities_space[x,2:3],border="red",lwd=3)
      return(View(data.frame(c(ruta,distancia))))
      break
    }
  }
}
