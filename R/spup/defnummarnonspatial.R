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

# Examples
# mynorm <- defnummarnonspatial(uncertain = TRUE, dist = "norm", par = c(70,3))

#logparams <- transformLog(0.120,0.250)
#mylog <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(logparams[2], logparams[1]))

#mybeta <- defnummarnonspatial(uncertain = TRUE, dist = "beta", par = c(50, 5))
