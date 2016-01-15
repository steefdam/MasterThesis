listRestructure <- function(matrix, n, q) {
  # convert matrix into list
  cvList <- c(matrix)
  
  # Initialize new list where the transformed list will be put into
  newlist <- vector(mode="numeric", length=50)
  
  # This is the start of the new list
  start <- (n/q/q*4)+1
  end <- n/q/q
  j <- 1
  for (i in start:end) {
    newlist[j] <- cvList[start]
    j <- j + 1
  }
}

newlist[1] <- soil_conssamps2[17]

start <- (50/5/5*4)+1
end <- 50/5
j <- 1
for (i in start:end) {
  newlist[j] <- soil_conssamps2[start]
  start <- start + 1
  j <- j + 1
}

soil_conssamps2[9]
soil_conssamps2[10] <- -4.13
