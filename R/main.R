# Stefan van Dam
# 26-01-2016

# Load libraries
library(sp)
library(rgdal)
library(gstat)
library(rgeos)

# Load functions
source("R/transformlog.R")
source("R/spup/defnummarnonspatial.R")
source("R/defSamples.R")

# Load Geul data
geul <- read.table("Data/geul/geuldata.txt", header = TRUE)
mask <- readGDAL("Data/geul/geul_mask.txt")
coordinates(geul) <- ~x+y

# Generate fitting variogram
gpb <- gstat(formula = pb~1, data = geul)
vgpb <- variogram(gpb, boundaries = c(0, 1:10*50, 600, 700, 800))
vgmpb <- vgm(nugget = 3000, psill = 27000, range = 170, model = "Exp")
vgmpb <- fit.variogram(vgpb, vgmpb, fit.method=7)
vgmpb2 <- vgm(psill = 14000, range = 150, model = "Sph", 
              add.to = vgm(psill = 16000, range = 500, model = "Sph"))
vgmpb2 <- fit.variogram(vgpb, vgmpb2, fit.method=7)

# Kriging and then plotting the Geul data
geul.krig <- krige(pb~1, geul, newdata = mask, vgmpb2)
spplot(geul.krig, zcol="var1.pred")


# Transform log parameters and then create an object of class nummarnonspatial
logparams <- transformLog(0.120,0.250)
myLog <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(logparams[1], logparams[2]))


# RANDOM SAMPLING
# Draw samples from each input, choose n and sample method
n <- 5
mySamplesLog <- defSamples(n = n, object = myLog, samplemethod = "random")

myNS <- defSamples(n = n, object = geul.krig, samplemethod = "random")


#PERFORM THE MC ANALYSIS
# create empty matrix with n columns and length of sp object rows
mcAnalysisRandom <- matrix(NA, ncol=n, nrow=length(geul.krig$var1.pred))

# Do the analysis
for (j in 1:n) {
  mcAnalysisRandom[,j] <- mySamplesLog[j] * myNS[,j]
}

# Retrieve the mean from the analysis
solutionRandom <- apply(mcAnalysisRandom, 1, mean)

# Put the mean into the spatial object
geul.krig$meanSRSrandom <- as.numeric(solutionRandom)

# Plot the object
spplot(geul.krig, zcol="meanSRSrandom")


# STRATIFIED SAMPLING
# Draw samples from each input, choose n and sample method
n <- 5
mySamplesLogStrat <- defSamples(n = n, object = myLog, samplemethod = "strat", p = 0:5/5)

myNSStrat <- defSamples(n = n, object = geul.krig, samplemethod = "strat", p = 0:5/5)


#PERFORM THE MC ANALYSIS
# Create empty matrix with n columns and length of sp object rows
mcAnalysisStrat <- matrix(NA, ncol=n, nrow=length(geul.krig$var1.pred))

# Do the analysis
for (j in 1:n) {
  mcAnalysisStrat[,j] <- mySamplesLogStrat[j] * myNSStrat[,j]
}

# Retrieve the mean from the analysis
solutionStrat <- apply(mcAnalysisStrat, 1, mean)

# Put the mean into the spatial object
geul.krig$meanSRSStrat <- as.numeric(solutionStrat)

# Plot the object
spplot(geul.krig, zcol="meanSRSStrat")


# Plot both
spplot(geul.krig, zcol=c(3,4))