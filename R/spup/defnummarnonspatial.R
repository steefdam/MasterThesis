# Kasia Sawicka & Stefan van Dam
# January 2016

# This functions takes a distribution wth its parameters as input and makes an s$ class of it

defnummarnonspatial <- function(uncertain, dist, par) {
  # set class
  setClass("nummarnonspatial",
           slots = list(uncertain = "logical",
                        dist = "character",
                        par = "numeric"))

  # create new object of class nummarspatial
  um <- new("nummarnonspatial", uncertain = uncertain, dist = dist, par = par)

  return(um)
}

# Example
mynorm <- defnummarnonspatial(uncertain = TRUE, dist = "norm", par = c(70,3))

mylog <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(0.120, 0.250))

mybeta <- defnummarnonspatial(uncertain = TRUE, dist = "beta", par = c(50, 5))

# myNS  <- uncertain = TRUE, dist = "norm", par = c(geul.krig$var1.pred,geul.krig$var1.var))
