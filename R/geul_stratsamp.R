# Stefan van Dam
# 6 January 2016

# Load functions
source("R/stratsamp.R")
source("R/transformLog.R")

# load libraries
library(foreach)

calcMeanSpatialSTRS <- function(n, SpatialMeans, SpatialVariances, nsmean, nssigma, p) {
  x <- foreach(a=SpatialMeans, b=SpatialVariances) %do% {
    stratsamp(a, b, n/5, 0:5/5)
  }
  
  y <- foreach(a=x) %do% {
    as.numeric(a)
  }
  
  STRSsamplemaps <- matrix(NA, nrow=length(SpatialMeans), ncol=n)
  i <- 1
  while (i <= length(SpatialMeans)) {
    STRSsamplemaps[i, ] <- y[i][[1]]
    i = i + 1
  }
  m <- nsmean
  s <- nssigma
  scale <- sqrt(log(s**2/m**2 + 1))
  locat <- log(m) -0.5 * scale^2
  soil_conssamps <- stratsamp(n, locat, scale)
  
  
  spb <- matrix(NA, ncol=n*n, nrow=length(SpatialMeans))
  
  i <- 1
  j <- 1
  k <- 1
  while (i <= n) {
    while (j <= n) {
      spb[, k] <- soil_conssamps[i] * STRSsamplemaps[, j]
      j <- j + 1
      k <- k + 1
    }
    j <- 1
    i <- i + 1
  }
  # Now the mean is calculated of each of these.
  solution <- apply(spb, 1, mean)
  return(solution)
}

STRS <- calcMeanSpatialSTRS(10, geul.krig$var1.pred, geul.krig$var1.var, 0.120, 0.250)

# The mean is added to the geul dataset in order to plot it
geul.krig$meanSTRS <- as.numeric(STRS)

# and the mean is plotted by spplot
spplot(geul.krig, zcol="meanSTRS")
