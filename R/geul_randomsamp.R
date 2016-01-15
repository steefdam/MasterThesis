# Stefan van Dam
# 6 January 2016

# For this script, it is assumed that R/geul_data.R has been run. That loads all necessary data.
# This geul dataset is the output of the krige function, which contains means and variances.
# Of each point in the dataset, n samples are randomly generated.
# These n samples of each point are put into a matrix,
# which has n columns and just as many rows as the dataset has points.
# Then the mean is calculated of all the samples and put into a list. 
# This list is added to the guel data set and plotted.

# Load necessary functions
source("R/randomsamp.R")

# Load libraries
library(foreach)

set.seed(123123)

calcMeanSpatialSRS <- function(n, SpatialMeans, SpatialVariances, nsmean, nssigma) {
  # An empty matrix is made to store all the 100 random realizations of each point in the geul data
  samplemaps <- matrix(NA, nrow=length(SpatialMeans), ncol=n)
  
  SpatialVariances <- sqrt(SpatialVariances)
  # Here the 100 realizations are made of each point in the geuldata and stored in the samplemaps.
  # The 100 realizations are stored in samplemaps
  for (i in 1:n) {
    x <- foreach(a=SpatialMeans, b=SpatialVariances) %do% {
      randomsamp(a, b, 1)
    }
    samplemaps[, i] <- as.numeric(x)
  }
  
  # Then we also sample from the other input, which is the soil consumption.
  # This is in log, so we have to transform it.
  log <- transformLog(0.120,0.250)
  soil_conssamps <- rlnorm(10, log[2], log[1])
  
  # Then we create a matrix, where the output is stored of the model, in this case I = S * Pb
  # S are the samples drawn put in soil_conssamps
  # Pb are the samplemaps
  # so spb contains the output of S * Pb
  # the first S will be multiplied by the first Pb, so the number of rows spb must have is n
  spb <- matrix(NA, ncol=n, nrow=length(SpatialMeans))
  
  for (j in 1:n) {
    spb[,j] <- soil_conssamps[j] * samplemaps[,j]
  }
  
  # Now the mean is calculated of each of these.
  solution <- apply(spb, 1, mean)
  return(solution)
}

SRS <- calcMeanSpatialSRS(5, geul.krig$var1.pred, geul.krig$var1.var, 0.120, 0.250)

# The mean is added to the geul dataset in order to plot it
geul.krig$meanSRS <- as.numeric(SRS)

# and then the mean is plotted by spplot
spplot(geul.krig, zcol="meanSRS")

spplot(geul.krig)



