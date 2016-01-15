
geul.krig$var1.var <- sqrt(geul.krig$var1.var)
x <- foreach(a=geul.krig$var1.pred, b=geul.krig$var1.var) %do% {
  as.numeric(stratsamp(a, b, 50/50, 0:5/5))
}

STRSsamplemaps <- matrix(NA, nrow=length(geul.krig$var1.pred), ncol=50)

for (i in 1:length(geul.krig$var1.pred)) {
  STRSsamplemaps[i, ] <- x[i][[1]]
}

log <- transformLog(0.120,0.250)
soil_conssamps <- stratsamp(log[2], log[1], 10, 0:5/5)

spb <- matrix(NA, ncol=50, nrow=length(geul.krig$var1.pred))

for (j in 1:n) {
  # deel n door q
  12*910
  34*23
}

i <- 1
j <- n/q/q

i <- 1
j <- 1
k <- 1
while (i <= n) {
  while (j <= n) {
    spb[, k] <- soil_conssamps[i] * STRSsamplemaps[, j]
    j <- j + 1
    k <- k + 1
  }
  j <- 1
  i <- i + 1
}
# Now the mean is calculated of each of these.
solution <- apply(spb, 1, mean)
return(solution)