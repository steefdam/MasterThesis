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


# Transform log parameters an then create an object of class nummarnonspatial
logparams <- transformLog(0.120,0.250)
myLog <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(logparams[2], logparams[1]))


# Draw samples from each input, choose n
n <- 1000
mySamplesLog <- defSamples(n = n, object = myLog)

myNS  <- defSamples(n = n, object = geul.krig)


#PERFORM THE MC ANALYSIS
# create empty matrix with n columns and length of sp object rows
mcAnalysis <- matrix(NA, ncol=n, nrow=length(geul.krig$var1.pred))

# Do the analysis
for (j in 1:n) {
  mcAnalysis[,j] <- mySamplesLog[j] * myNS[,j]
}

# Retrieve the mean from the analysis
solution <- apply(mcAnalysis, 1, mean)

# Put the mean into the spatial object
geul.krig$meanSRS <- as.numeric(solution)

# Plot the object
spplot(geul.krig, zcol="meanSRS")
