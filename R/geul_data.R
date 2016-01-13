# Load libraries
library(sp)
library(rgdal)
library(gstat)
library(rgeos)

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

# Krige the Geul data
geul.krig <- krige(pb~1, geul, newdata = mask, vgmpb2)
spplot(geul.krig, zcol="var1.pred")

