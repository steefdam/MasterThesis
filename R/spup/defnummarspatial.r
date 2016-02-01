defnummarspatial <- function(uncertain, SpatialObject, mask, semivar = NA, beta = NA) {
  # check if required packages are loaded
  require(gstat)
  require(sp)

  # check if SpatialObject contains data
  if (is.null(SpatialObject@data)) # This must not have a ! :)
    stop("SpatialObject has to contain data")

  # check if SpatialObject is spatial
  allSpatialObjects <- c("SpatialPointsDataFrame", "SpatialGridDataFrame")
  if (!(class(SpatialObject) %in% allSpatialObjects))
    stop("SpatialObject has to be of class Spatial")

  # check if mask is a SpatialGridDataFrame
  if (class(mask) != "SpatialGridDataFrame")
    stop("mask has to be of class SpatialGridDatFrame")

  # check if SpatialObject and mask has same coordinate reference system. Look up Krige
  if (!identical(SpatialObject@proj4string, mask@proj4string))
    stop("Data item in SpatialObject and mask have different coordinate reference systems")

  # check if semivar is a real variogramModel
  if (class(semivar)[1] != "variogramModel")
    stop("semivar has to be a valid variogram model")

  # x and y are needed to realize the class. But: is this elegant in R?
  x <- class(SpatialObject)[1]
  
  # setClassUnion("semivar", c(class(semivar)[1], class(semivar)[2]))
  semivar <- unclass(semivar)
  semivar <- as.data.frame(semivar)
  print(class(semivar))
  # setClass("wowie", representation(model = "variogramModel"), contains = "data.frame")
  # set class
  setClass("nummarspatial",
           slots = list(uncertain = "logical",
                        SpatialObject = x,
                        mask = "SpatialGridDataFrame",
                        semivar = "data.frame"))
  # create new object of class nummarspatial
  um <- new("nummarspatial", uncertain = uncertain, SpatialObject = SpatialObject, mask = mask, semivar = semivar)

  return(um)
}

# Example
a <- defnummarspatial(uncertain = TRUE, SpatialObject = geul, mask = mask, semivar = vgmpb)
# a
# # # class(geul)
#  class(vgmpb3)
# vgmpb3 <- unclass(vgmpb)
# y <- class(vgmpb)[1]
# z <- class(vgmpb)[2]
# c(y,z)
# class(vgmpb)
# # class(vgmpb)[1]
# setClassUnion("semivar", c("vector", "data.frame"))
# setClassUnion("semivar", class(vgmpb))
