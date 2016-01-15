library(foreach)
set.seed(123123)
# An empty matrix is made to store all the 100 random realizations of each point in the geul data
samplemaps <- matrix(NA, nrow=length(geul.krig$var1.pred), ncol=5)

# Here the 100 realizations are made of each point in the geuldata and stored in the samplemaps.
# The 100 realizations are stored in samplemaps
for (i in 1:5) {
  x <- foreach(a=geul.krig$var1.pred, b=geul.krig$var1.var) %do% {
    randomsamp(a, b, 1)
  }
  samplemaps[, i] <- as.numeric(x)
}

  samplemaps2 <- foreach(a=geul.krig$var1.pred, b=geul.krig$var1.var, .combine=rbind) %do% {
    randomsamp(a, b, 10)
  }


# Then we also sample from the other input, which is the soil consumption.
# This is in log, so we have to transform it.
m <- 0.12
s <- 0.250
scale <- sqrt(log(s**2/m**2 + 1))
locat <- log(m) -0.5 * scale^2
soil_conssamps <- rlnorm(5, locat, scale)

# Then we create a matrix, where the output is stored of the model, in this case I = S * Pb
# S are the samples drawn put in soil_conssamps
# Pb are the samplemaps
# so spb contains the output of S * Pb
# the first S will be multiplied by the first Pb, so the number of rows spb must have is n
spb <- matrix(NA, ncol=5, nrow=length(geul.krig$var1.pred))

for (j in 1:5) {
  spb[,j] <- soil_conssamps[j] * samplemaps[,j]
}

# Now the mean is calculated of each of these.
solution <- apply(spb, 1, mean)
return(solution)

SRS <- calcMeanSpatialSRS(10, geul.krig$var1.pred, geul.krig$var1.var, 0.120, 0.250)

# The mean is added to the geul dataset in order to plot it
geul.krig$meanSRS <- as.numeric(solution)

# and the mean is plotted by spplot
spplot(geul.krig, zcol="meanSRS")

spplot(geul.krig)


##################Rommel#################

