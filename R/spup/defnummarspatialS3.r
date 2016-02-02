defnummarspatial <- function(uncertain = FALSE, SpatialObject = NULL, mask = NULL, semivar = NULL, beta = NA) {
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
  if (!is.null(semivar)) {
    if (class(semivar)[1] != "variogramModel")
      stop("semivar has to be a valid variogram model")
  }
  
  um <- list(uncertain = uncertain,
             SpatialObject = SpatialObject,
             mask = mask,
             semivar = semivar,
             beta = beta)
    
  ## Set the name for the class
  class(um) <- "nummarspatial"
  return(um)
}

# Example
a <- defnummarspatial(uncertain = TRUE, SpatialObject = geul, mask = mask, semivar = vgmpb)

class(a)





