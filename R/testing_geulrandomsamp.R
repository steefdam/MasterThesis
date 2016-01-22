library(foreach)
set.seed(123123)

# Here the 100 realizations are made of each point in the geuldata and stored in the samplemaps.
# The 100 realizations are stored in samplemaps

geul.krig$var1.var <- sqrt(geul.krig$var1.var)

n <- 10

samplemaps <- foreach(a = geul.krig$var1.pred, b = geul.krig$var1.var, .combine = rbind) %do% {
  randomsamp(a, b, n)
}

log <- transformLog(0.120, 0.250)
soil_conssamps <- rlnorm(n, log[2], log[1])

spb <- matrix(NA, ncol=n, nrow=length(geul.krig$var1.pred))

for (j in 1:n) {
  spb[,j] <- soil_conssamps[j] * samplemaps[,j]
}

solution <- apply(spb, 1, mean)

geul.krig$meanSRS <- as.numeric(solution)

spplot(geul.krig, zcol="meanSRS")



# Then we also sample from the other input, which is the soil consumption.
# This is in log, so we have to transform it first.
m <- 0.12
s <- 0.250
scale <- sqrt(log(s**2/m**2 + 1))
locat <- log(m) -0.5 * scale^2
soil_conssamps <- rlnorm(n, locat, scale)

# Then we create a matrix, where the output is stored of the model, in this case I = S * Pb
# S are the samples drawn put in soil_conssamps
# Pb are the samplemaps
# so spb contains the output of S * Pb
# the first S will be multiplied by the first Pb, so the number of rows spb must have is n
spb <- matrix(NA, ncol=n, nrow=length(geul.krig$var1.pred))

for (j in 1:n) {
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

spplot(geul.krig, zcol=c(1,3))

spplot(geul.krig)


##################Rommel#################

