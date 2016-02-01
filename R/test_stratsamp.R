# Stefan van Dam
# 14-01-2016

# test_stratsamp <- function(object, n, p) {
#   # Note that here p is a vector of probs
#   # n is sample size per stratum
#   # mu and sigma are parameters of normal distribution to be sampled
#   # function returns matrix with n rows and length(p)-1 columns
#   samples <- foreach(a = object[[1]], b = sqrt(object[[2]])) %do% {
#     if (is.na(a) || (is.na(b))) {
#       outmat <- matrix(NA, n, length(p)-1)
#     } else {
#       lims <- qnorm(p, a, b)
#       outmat <- matrix(NA, n, length(p)-1)
#       counts <- rep(0, length(lims)-1)
#       while(any(counts < n)) {
#         r <- rnorm(1, a, b)
#         intvl <- findInterval(r,lims)
#         if(counts[intvl] < n) {
#           counts[intvl] <- counts[intvl] + 1
#           outmat[counts[intvl], intvl] <- r
#         }
#       }
#     }
#   }
#   as.matrix(samples)
#   return(samples)
# }

test_stratsamp <- function(mu, sigma, n, p) {
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

geul.krig$var1.var <- sqrt(geul.krig$var1.var)
x <- foreach(a=geul.krig$var1.pred, b=geul.krig$var1.var) %do% {
  as.numeric(test_stratsamp(a, b, 5/5, 0:5/5))
}

STRSsamplemaps <- matrix(NA, nrow=length(geul.krig$var1.pred), ncol=5)

for (i in 1:length(geul.krig$var1.pred)) {
  STRSsamplemaps[i, ] <- x[i][[1]]
}

x <- test_stratsamp(object = geul.krig, 10, 0:5/5)
x[1]
