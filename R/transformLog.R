# Stefan van Dam
# 14-01-2015

transformLog <- function(m,s) {
  scale <- sqrt(log(s**2/m**2 + 1))
  locat <- log(m) -0.5 * scale^2
  both <- c(scale, locat)
  return(both)
}