source("R/randomsamp.R")

library(foreach)

defSamples <- function(n, object, samplemethod = "random", q = 0) {
  if (samplemethod == "random") {
    # this is the tricky part
    if (class(object) == "SpatialGridDataFrame") {
      x <- foreach(a = object[[1]], b = object[[2]], .combine = rbind) %do% {
        randomsamp(a, b, n)
      }
    } 
    
    if (class(object) == "nummarnonspatial") {
      if (object@dist == "norm") {
        x <- rnorm(n, object@par[1], object@par[2])
      }
      
      if (object@dist == "log") {
        x <- rlnorm(n, object@par[1], object@par[2])
      }
      
      if (object@dist == "beta") {
        x <- rbeta(n, object@par[1], object@par[2])
      }
    }
  }
  
  if (samplemethod == "strat") {
    
  }
  
  return(x)
}

mySamplesNorm <- defSamples(n = 10, object = mynorm)

mySamplesLog <- defSamples(n = 10, object = mylog)

mySamplesBeta <- defSamples(n = 10, object = mybeta)

myNS  <- defSamples(n = 10, object = geul.krig)
