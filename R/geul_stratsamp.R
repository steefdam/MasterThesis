# Stefan van Dam
# 6 January 2016

stratsamp <- function(mu, sigma, n, p) {
  # Note that here p is a vector of probs
  # n is sample size per stratum
  # mu and sigma are parameters of normal distribution to be sampled
  # function returns matrix with n rows and length(p)-1 columns
  if (is.na(mu) || (is.na(sigma))) {
    outmat <- matrix(NA, n, length(p)-1)
  } else {
    lims <- qnorm(p, mu, sigma)
    outmat <- matrix(NA, n, length(p)-1)
    counts <- rep(0, length(lims)-1)
    while(any(counts < n)){
      r <- rnorm(1, mu, sigma)
      intvl <- findInterval(r,lims)
      if(counts[intvl] < n){
        counts[intvl] <- counts[intvl] + 1
        outmat[counts[intvl], intvl] <- r
      }
    }
  }
  return(outmat)
}

library(foreach)

calcMeanSpatialSTRS <- function(n, SpatialMeans, SpatialVariances, nsmean, nssigma) {
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
  
  soil_conssamps <- rlnorm(n, nsmean, nssigma)
  
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
