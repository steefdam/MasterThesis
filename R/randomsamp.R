# Stefan van Dam
# 14-01-2016

# This function is a nested rnorm function.
# Can be extended with other distirbutions
randomsamp <- function(mu, sigma, n) {
  if(is.na(mu) || is.na(sigma))
    samples <- rep(NA, n) else
  samples <- rnorm(n, mu, sigma)
  return(samples)
}