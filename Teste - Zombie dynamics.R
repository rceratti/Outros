# Function to create initial population
initialPop <- function(n, ab1, ab2, infected.prop) {
  pop <- data.frame(id = 1:n, ability = rbeta(n, ab1, ab2), status = 'A',
                    stringsAsFactors = FALSE)

  # Creating infected individuals
  infected.id <- sample(pop$id, round(n * infected.prop))
  pop$status[infected.id] <- 'Z'

  pop
}



# Function to pair up individuals
pairUp <- function(data) {
  # Pair up those alive and zombies
  npairs <- floor(nrow(data)/2)
  pairs <- sample(1:(2*npairs))
  
  # Pairs of zombies vs humans
  p1 <- pairs[1:npairs]
  p2 <- pairs[(npairs + 1):(2 * npairs)]
  index.ZvH <- data$status[p1] != data$status[p2]

  # Returns pairs of interest
  pairs <- cbind(p1, p2)
  matrix(pairs[index.ZvH, ], ncol = 2)
}



# Function to simulate encounter
encounter <- function(data, pairs) {
  if(nrow(pairs) == 0)
    return(data)

  status.Z <- data$status[pairs[, 1]] == 'Z'
  pairs.tmp <- pairs
  pairs[status.Z, 1] <- pairs.tmp[status.Z, 2]
  pairs[status.Z, 2] <- pairs.tmp[status.Z, 1]

  humanWin <- rbinom(nrow(pairs), 1, data$ability[pairs[, 1]])
  data$status[pairs[humanWin == 0, 1]] <- 'Z'
  data[- pairs[humanWin == 1, 2], ]
}



# Function to simulate outbreak
outbreak <- function(population, rounds) {
  results <- matrix(0, rounds, 2)
  j <- 1
  nh <- sum(population$status == 'A')
  nz <- nrow(population) - nh
  results[j, ] <- c(nh, nz)
   
  while(nh > 0 & nz > 0 & j < rounds) {
    j <- j+1
    pairs <- pairUp(population)
    population <- encounter(population, pairs)
    nh <- sum(population$status == 'A')
    nz <- nrow(population) - nh
    results[j, ] <- c(nh, nz)
  }

  results[rowSums(results) > 0, ]
}



# 
n <- 1e5
ab1 <- 15; ab2 <- 15  
x <- seq(0, 1, l = 1e2); plot(x, dbeta(x, ab1, ab2), ty = 'l')
infected.prop <- .025

pop <- initialPop(n, ab1, ab2, infected.prop)
tes <- outbreak(pop, 150)
tes <- cbind(tes, n - rowSums(tes))

matplot(1:nrow(tes), tes/n, ty = 'l', xlab = "Iterations", ylab = "Proportion")
legend("topright", c('Alive', 'Zombie', 'Dead'), col = 1:3, lty = 1:3)