source("R/randomsamp.R")
source("R/stratsamp.R")

library(foreach)

defSamples <- function(n, object, samplemethod = "random", p = 0) {
  if (samplemethod == "random") {
    # this is the tricky part
    if (class(object) == "SpatialGridDataFrame") {
      samples <- foreach(a = object[[1]], b = sqrt(object[[2]]), .combine = rbind) %do% {
        randomsamp(a, b, n)
      }
    }
    
    if (class(object) == "nummarnonspatial") {
      if (object@dist == "norm") {
        samples <- rnorm(n, object@par[1], object@par[2])
      }
      
      if (object@dist == "log") {
        samples <- rlnorm(n, object@par[1], object@par[2])
      }
      
      if (object@dist == "beta") {
        samples <- rbeta(n, object@par[1], object@par[2])
      }
    }
  }
  
  if (samplemethod == "strat") {
    if (class(object) == "SpatialGridDataFrame") {
      x <- foreach(a = object[[1]], b = sqrt(object[[2]]), .combine = rbind) %do% {
        as.numeric(stratsampSpatial(a, b, n/n, p))
      }

      samples <- matrix(NA, nrow=length(object[[1]]), ncol=n)
      
      for (i in 1:length(object[[1]])) {
        samples[i, ] <- x[i][[1]]
      }
    }
    
    if (class(object) == "nummarnonspatial") {
      samples <- stratsamp(object, n, p)
      samples <- sampleReshuffle(samples)
    }
  }
  return(samples)
}

# mySamplesNorm <- defSamples(n = 10, object = mynorm)
# 
# mySamplesLog <- defSamples(n = 10, object = mylog)
# 
# mySamplesBeta <- defSamples(n = 10, object = mybeta)
# 
# myNS  <- defSamples(n = 10, object = geul.krig)
